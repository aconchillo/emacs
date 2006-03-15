;; ma-objc-dox.el -- Doxygen documentation templates
;; Start date: 13/03/05 (jao)
;; (c) MacAvity Software 2005

;;; Commentary:

;; This file provides functions for automatic insertion of doxygen
;; comments in objective-C. Installing it is just a matter of (require
;; 'ma-objc-dox)

;; Public functions:
;;
;; The two key commands of this package are:
;;
;; - `ma-objc-dox' This command will search for the nearest interface,
;;   global variable, method or enumeration definition, ask for a
;;   brief description and insert a doxygen comment skeleton. You
;;   don't need to be in the exact line of the declaration: it will
;;   try to find it, and will highlight the declaration line (with a
;;   customisable face).
;;
;; - `ma-objc-dox-file' Locate the point at the beginning of your
;;   header and invoke this command: a standard doxygen file comment
;;   will be inserted.
;;
;; Other commands:
;;
;; - `ma-objc-dox-group' includes the current region in a doxygen
;;   block, asking for its title.
;;
;; - `ma-objc-dox-inline' inserts an inline doxygen comment (beginning
;;   with '/**< .. */') in the line at point.
;;
;; - `ma-objc-dox-{class, variable, enum}' insert the corresponding
;;   doxygen comment, assuming the cursor is on a line declaring an
;;   appropriate entity.
;;

;;; TODO:
;;
;;  * Insert comments for typedefs.
;;  * When adding a pointer ivar, release it in dealloc.
;;

;;; Code:

(require 'cl) ;; reduce
(require 'ma-objc-common)

;; Customization:
(defgroup ma-objc-dox nil
  "Doxygen comments for Objective-C"
  :group 'ma-objc)

(defface ma-objc-dox-highlight-face
  '((t (:background "grey70")))
  "Face used to highlight comment insertion line."
  :group 'ma-objc-dox)

;; Public functions:

(defun ma-objc-dox ()
  "Insert doxygen comments for nearest element"
  (interactive)
  (beginning-of-line)
  (let* ((none '(lambda () (message "No comment insertion point found")))
         (end (+ (point-max) 1))
         (pos (mapcar '(lambda (x)
                         (cons
                          (save-excursion
                            (or (re-search-forward (car x) nil t) end))
                          (cdr x)))
                      (list (cons %ma-objc-dox-class-regexp%
                                  'ma-objc-dox-class)
                            (cons %ma-objc-dox-method-regexp%
                                  'ma-objc-dox-scan-method)
                            (cons %ma-objc-dox-enum-regexp%
                                  'ma-objc-dox-enum)
                            (cons %ma-objc-dox-other-regexp%
                                  'ma-objc-dox-desc))))
         (winner (reduce '(lambda (a b) (if (<= (car a) (car b)) a b))
                         pos :initial-value (cons end none))))
    (goto-char (car winner))
    (let* ((beg (progn (beginning-of-line) (point)))
           (end (progn (forward-line) (point)))
           (ovr (make-overlay beg end)))
      (overlay-put ovr 'face 'ma-objc-dox-highlight-face)
      (forward-line -1)
      (funcall (cdr winner))
      (delete-overlay ovr))))

(defun ma-objc-dox-file ()
  "Insert doxygen comments for a file header"
  (interactive)
  (let ((desc (if (string= (file-name-extension (buffer-file-name)) "m")
                  (format "Implementation of %s" (ma-objc-buffer-class))
                (read-string "Brief file description: "))))
    (ma-objc-do-dox-file desc)))

(defun ma-objc-dox-class (&optional desc)
  "Insert doxygen comments for a class interface"
  (interactive)
  (ma-objc-dox-desc desc))

(defun ma-objc-dox-variable (&optional desc)
  "Insert doxygen comments for a global variable declaration"
  (interactive)
  (ma-objc-dox-desc desc))

(defun ma-objc-dox-enum ()
  "Insert doxygen comments for an enumeration"
  (interactive)
  (ma-objc-dox-desc)
  (save-excursion
    (re-search-forward "{" nil t)
    (let ((ll (- (line-number-at-pos
                  (save-excursion (re-search-forward "}")))
                 1))
          (ovr (make-overlay 0 0)))
      (overlay-put ovr 'face 'ma-objc-dox-highlight-face)
      (while (< (line-number-at-pos) ll)
          (forward-line)
          (move-overlay ovr (progn (beginning-of-line) (point))
                        (save-excursion (forward-line) (point)))
          (ma-objc-dox-inline))
      (delete-overlay ovr))))

(defun ma-objc-dox-inline (&optional contents)
  "Insert a doxygen comment in the same line"
  (interactive)
  (end-of-line)
  (ma-objc-dox-oneliner (or contents (read-string "Comment: ")))
  (indent-for-comment))

(defun ma-objc-dox-group ()
  "Wrap region in a group comment"
  (interactive)
  (save-excursion
    (let* ((beg (region-beginning))
           (end (region-end))
           (len (- end beg)))
      (goto-char beg)
      (beginning-of-line)
      (insert "/** \\name " (read-string "Group's name: ") "\n")
      (insert "<Group description>\n\n*/\n//@{\n")
      (forward-char len)
      (beginning-of-line)
      (insert "//@}\n")
      (indent-region beg (point)))))

;; Private stuff:

(defconst %ma-objc-dox-class-regexp% "^\\(@\\(interface\\|protocol\\)\\) ")
(defconst %ma-objc-dox-method-regexp% "^[-+] *([A-Za-z][A-Za-z0-9_]*) ")
(defconst %ma-objc-dox-enum-regexp% "^ *\\(typedef \\)? *enum\\( \\|$\\)")
(defconst %ma-objc-dox-other-regexp% "^[A-Za-z][A-Za-z0-9]* ")

(defmacro ma-objc-dox-block (&rest body)
  (let ((ins00 (mapcar
              '(lambda (l) (append (list 'insert "\n   ") l)) body)))
    `(progn (insert "/**") ,@ins00 (insert "\n*/\n"))))

(defsubst ma-objc-dox-begin () (insert "/**\n"))
(defsubst ma-objc-dox-end () (insert "\n*/\n"))
(defsubst ma-objc-dox-oneliner (txt) (insert "/**< " txt " */"))

(defun ma-objc-dox-desc (&optional desc)
  "Insert a doxygen block asking for a brief description"
  (let ((desc (or desc (read-string "Brief description: "))))
    (beginning-of-line)
    (ma-objc-dox-block
     (desc ".")
     ("<Detailed description>"))))

(defun ma-objc-dox-method-x (params retp)
  "Insert doxygen comments for a method declaration, given a param list
and whether it returns a value"
  (beginning-of-line)
  (let ((desc (read-string "Brief description: ")))
    (ma-objc-dox-begin)
    (insert "    " desc ".\n    <Detailed description>\n\n")
    (mapc '(lambda (p) (insert "    \\param " p " "
                               (read-string (format "Description for '%s': " p))
                               "\n"))
          params)
    (if retp (insert "    \\return "
                     (read-string "Return value's description: ")))
    (ma-objc-dox-end)))

(defun ma-objc-dox-scan-method ()
  "Scan params and return value of method at pos, and insert comment"
  (beginning-of-line)
  (let* ((end (save-excursion (re-search-forward ";")))
         (params nil)
         (retp (not (save-excursion (re-search-forward "^[-+] *(void)" end t)))))
    (save-excursion
      (while (re-search-forward ": *([^)]+) *\\([^: ;\n]+\\)" end t)
        (setq params (cons (match-string 1) params))))
    (ma-objc-dox-method-x (nreverse params) retp)
    ;; for use by callers elsewhere
    (cons retp params)))

(defun ma-objc-do-dox-file (desc)
  (beginning-of-line)
  (ma-objc-dox-block
   ("\\file " (file-name-nondirectory (buffer-file-name)))
   ("\\author " (user-full-name) " <" user-mail-address ">")
   ("\\date " (format-time-string "%a %b %d, %Y %H:%M"))
   ("\\brief " desc))
  (insert "\n// Copyright (c) " (format-time-string "%Y MacAvity Software.")
          " All rights reserved.\n\n\n"))

(provide 'ma-objc-dox)

;;; ma-objc-dox.el ends here
