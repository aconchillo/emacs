;; ma-objc-utils.el -- utility functions for Objective-C
;; Start date: 12/03/05 (jao)
;; (c) MacAvity Software 2005

;;; Commentary:
;;
;; This file provides some utility functions for Objective-C
;; programming. Just put it in your load path and (require
;; 'ma-objc-utils).
;; This file depends on ma-objc-common.el and ma-objc-dox.el.
;;
;;;; Public functions:
;;
;; This package defines the following interactive functions
;;
;; - `ma-objc-insert-method' In a header file, put the cursor in the
;;   method declaration that will go after the new one. Invoke this
;;   command. You'll be asked for the new method declaration (write it
;;   in full, without the trailing semicolon). The declaration will be
;;   inserted in the header, and an empty definition in the
;;   corresponding .m file (located at the correct place), which will
;;   be open in another window.
;;
;; - `ma-objc-insert-ivar' Call this function with the cursor in the
;;   interface declaration of your class, at the place where you want
;;   your new ivar declared. You'll be asked for the variable's name,
;;   base type, and whether it's a pointer to the base type or not.
;;   E.g., if you want to add an 'NSString * foo_' ivar, your answers
;;   should be, resp., 'foo_', 'NSString' and 'y'.
;;
;;   The variable declaration is inserted in your .h, but the fun
;;   does not end here: next, you'll be asked for the getter style
;;   (simple or copy), and for the setter style (retain/release or
;;   release/copy) -- see our wiki page on coding standards for
;;   details on these accessor styles. After you have chosen those
;;   styles, the corresponding method declarations will be inserted
;;   in the 'PrivateAPI' category of the source file (.m), and the
;;   definitions in the '@implementation' section. If any of these
;;   sections (or the .m file itself) are missing, they're created.
;;
;; The following functions are useful when you already have the ivars
;; declared and/or want to choose where the accessor
;; declarations/definitions are inserted:
;;
;; - `ma-objc-insert-decls' Copy to the kill ring (with M-w or the
;;   mouse) a region containing the declarations of the ivars that
;;   interest you. Position the cursor at the point, in any buffer,
;;   where you want the setter and getter declarations to be
;;   inserted. Call this function. You'll be asked about the
;;   corresponding styles, and the declarations will be inserted
;;   right there.
;;
;; - `ma-objc-insert-defs' This function works like the previous one,
;;   but inserts definitions instead of declarations.
;;
;; - `ma-objc-insert-dealloc' Once you're happy with your ivar
;;   definitions, M-w their declaration, go to the point in your .m
;;   file where you want dealloc inserted and invoke this command.
;;
;; Finally, some other useful insertions:
;;
;; - `ma-objc-insert-private-category' Inserts an empty private API
;;   category at point, named after the custom variable
;;   `ma-objc-private-category-name' (PrivateAPI by default).
;;
;; - `ma-objc-insert-implementation' Inserts an empty @implementation
;;   section at point.
;;
;; - `ma-objc-insert-protocol-defs' Inserts at point empty definitions
;;   for all methods in a given protocol, after asking for its name.
;;
;; - `ma-objc-create-class' Asks for a classname and creates its .h
;;   and .m files with adequate doxygen comments.
;;
;; - `ma-objc-create-uktest' Creates a new UnitKit test class for the
;;   current's buffer class.
;;

;;; Code:
(require 'ma-objc-common)
(require 'ma-objc-dox)

;;; Customisation:

(defvar ma-objc-offset-length 4 "Offset for indentation")
(defvar ma-objc-offset (make-string ma-objc-offset-length ?\ )
  "Offset for indentation string")
(defun ma-objc-set-indent-offset (length)
  "Call this function to customize the indentation offset"
  (setq ma-objc-offset-length length)
  (setq ma-objc-offset (make-string ma-objc-offset-length ?\ )))

;;; Public functions:

(defun ma-objc-insert-decls ()
  "Insert setter and getters declarations for variables in the kill ring"
  (interactive)
  (ma-objc-insert-decls-for-ivars (ma-objc-scan-ivars (current-kill 0))))

(defun ma-objc-insert-defs ()
  "Insert setter and getters definitions for variables in the kill ring"
  (interactive)
  (ma-objc-insert-defs-for-ivars (ma-objc-scan-ivars (current-kill 0))))

(defun ma-objc-insert-dealloc ()
  "Insert dealloc definition for variables in the kill ring"
  (interactive)
  (ma-objc-insert-dealloc-for-ivars (ma-objc-scan-ivars (current-kill 0))))

(defun ma-objc-insert-ivar ()
  (interactive)
  "Inserts an ivar declaration, its setters and its getters"
  (let ((name (read-string "Variable name: "))
        (type (read-string "Base type (no *): "))
        (pointerp (y-or-n-p "Is a pointer? "))
        (next-var (ma-objc-scan-ivar-at-point)))
    (beginning-of-line)
    (insert ma-objc-offset type (if pointerp " *" " ") name ";")
    (end-of-line)
    (insert "\n")
    (let ((buff (find-file-noselect (ma-objc-other-buffer)))
          (ivar (list name type pointerp)))
      (save-current-buffer
        (set-buffer buff)
        (save-excursion
          (ma-objc-find-ivar-decl next-var)
          (ma-objc-insert-decls-for-ivars (list ivar))
          (ma-objc-find-ivar-def next-var)
          (ma-objc-insert-defs-for-ivars (list ivar)))))))

(defun ma-objc-insert-method ()
  "Insert method declaration and definition skeleton"
  (interactive)
  (beginning-of-line)
  (let ((prev (save-excursion
                (if (re-search-forward %ma-objc-method-regexp% nil t)
                    (ma-objc-make-method-regexp (match-string 1))
                  (concat "^\\("
                          (regexp-quote ma-objc-private-section-title)
                          "\\|@end\\)"))))
        (new (read-string "Full method declaration: ")))
    (while (not (string-match %ma-objc-method-regexp% new))
      (setq new (read-string "Invalid declaration. Please insert new: " new)))
    (insert new ";")
    (let ((retp (car (ma-objc-dox-scan-method))))
      (end-of-line)
      (insert "\n")
      (find-file-other-window (ma-objc-other-buffer))
      (goto-char (point-min))
      (when (not (re-search-forward "^@implementation "))
        (goto-char (point-max))
        (insert "\n")
        (ma-objc-insert-implementation))
      (re-search-forward prev nil t)
      (beginning-of-line)
      (let ((pos (point)))
        (insert new "\n{\n"
                (or ma-objc-default-method-body "") "\n"
                (if retp "return nil;" "") "\n}\n\n")
        (indent-region pos (point) nil))
      (forward-line -3))))

(defun ma-objc-insert-protocol-defs ()
  "Insert definitions for a given protocol"
  (interactive)
  (let* ((proto (file-name-sans-extension (read-string "Protocol name: ")))
         (header (concat proto ".h"))
         (hfile (expand-file-name
                 (if (file-exists-p header) header
                   (read-file-name (format "Where is %s's header? " proto)
                                   nil "" t))))
         (methods '())
         (wc (current-window-configuration)))
    (save-current-buffer
      (find-file hfile)
      (save-excursion
        (goto-char (point-min))
        (when (re-search-forward "^@protocol ")
          (while (re-search-forward %ma-objc-method-regexp% nil t)
            (setq methods (cons (match-string 0) methods))))))
    (beginning-of-line)
    (if methods
        (mapc (lambda (d) (insert (substring d 0 -1) "\n{\n\n}\n\n"))
              (nreverse methods))
      (message "Protocol %s has no methods!" proto))
    (set-window-configuration wc)))

(defun ma-objc-insert-private-category ()
  "Insert a private api category at point"
  (interactive)
  (beginning-of-line)
  (insert "@interface " (ma-objc-buffer-class) " ("
	  ma-objc-private-category-name
          ")\n\n@end\n\n")
  (forward-line -2))

(defun ma-objc-insert-implementation ()
  "Insert a implementation section at point"
  (interactive)
  (beginning-of-line)
  (insert "@implementation " (ma-objc-buffer-class)
          "\n\n" ma-objc-private-section-title "\n\n\n@end\n\n")
  (forward-line -2))

(defun ma-objc-create-class ()
  "Create a new Objective-C class"
  (interactive)
  (let* ((class (read-string "Class name: "))
         (parent (read-string "Parent class (default NSObject): "
                              nil nil "NSObject"))
         (protocols (read-string "Protocols implemented (return for none): "))
         (desc (read-string "Brief file description: "))
         (plist (if (not (zerop (length protocols)))
                    (ma-objc-split-string protocols "[ ,\f\t\n\r\v]+" t)
                  nil)))
    (ma-objc-do-create-class class parent plist '("<Cocoa/Cocoa.h>") desc)))

(defun ma-objc-create-uktest ()
  "Create a UnitKit test case for the current class"
  (interactive)
  (let* ((class (ma-objc-buffer-class))
         (test-class (concat class "Test"))
         (parent "NSObject")
         (plist '("UKTest"))
         (headers `("<Foundation/Foundation.h>" "<UnitKit/UnitKit.h>"
                    ,(concat "\"" class ".h\"")))
         (desc (concat class " tests")))
    (ma-objc-do-create-class test-class parent plist headers desc t)))

;;; Private functions:

(defconst %ma-objc-ivar-decl-regexp%
  "^ *\\([A-Za-z][A-Za-z0-9_]*\\) *\\(\\*?\\) *\\([A-Za-z0-9_]+\\);$")

(defconst %ma-objc-method-regexp%
  "^\\(\\(\\+\\|-\\) *([^)]+)\\( *[^;]+[ \\n]*\\)\\)\\(;\\|\\n{\\n\\)?$")

(defsubst ma-objc-ivar-name (ivar) (nth 0 ivar))
(defsubst ma-objc-ivar-type (ivar) (nth 1 ivar))
(defsubst ma-objc-ivar-pointerp (ivar) (nth 2 ivar))

(defun ma-objc-make-setter-decl (name type pointerp)
  "Compose a set declaration for the given ivar and type"
  (concat "- (void) set" (upcase-initials name) ":"
          (ma-objc-make-param name type pointerp)))

(defun ma-objc-make-getter-decl (name type pointerp)
  "Compose a get declaration for the given ivar and type."
  (concat "- (" type (if pointerp " *) " ") ") name))

(defun ma-objc-make-param-name (name type)
  "Make an ivar setter parameter name"
  (concat ma-objc-param-prefix
          (upcase-initials (if (string= (substring name -1) "_")
                               (substring name 0 -1)
                             name))))

(defun ma-objc-make-param (name type pointerp)
  "Make an ivar setter parameter decl"
  (concat "(" type (if pointerp " *) " ") ")
          (ma-objc-make-param-name name type)))

(defun ma-objc-getter-decl (name type pointerp)
  "Compose a get declaration for the given ivar and type.

`pointerp` denotes whether the variable is a pointer."
  (concat (ma-objc-make-getter-decl name type pointerp) ";"))

(defun ma-objc-setter-decl (name type pointerp)
  "Compose a set declaration for the given ivar and type.

`pointerp` denotes whether the variable is a pointer."
  (concat (ma-objc-make-setter-decl name type pointerp) ";"))

(defun ma-objc-getter-simple (name type pointerp)
  "Compose a simple getter definition"
  (concat (ma-objc-make-getter-decl name type pointerp) "\n{\n    return "
          name ";\n}"))

(defun ma-objc-getter-copy (name type pointerp)
  "Compose a copy getter definition"
  (if (not pointerp)
      (ma-objc-getter-simple name type pointerp)
    (concat (ma-objc-make-getter-decl name type pointerp)
            "\n{\n"  ma-objc-offset  "return "
            "[[" name " copy] autorelease];\n}")))

(defun ma-objc-setter-not-pointer (name type &optional ignored)
  "Compose a setter for a builtin type"
  (let ((pname (ma-objc-make-param-name name type)))
    (concat (ma-objc-make-setter-decl name type nil)
            "\n{\n" ma-objc-offset name " = " pname ";\n}")))

(defun ma-objc-setter-retain-release (name type pointerp)
  "Compose a retain/release setter"
  (if (not pointerp)
      (ma-objc-setter-not-pointer name type)
    (let ((pname (ma-objc-make-param-name name type)))
      (concat (ma-objc-make-setter-decl name type pointerp)
              "\n{\n" ma-objc-offset "[" pname " retain];\n"
              ma-objc-offset "[" name " release];"
              "\n" ma-objc-offset name " = " pname ";\n}"))))

(defun ma-objc-setter-release-copy (name type pointerp)
  "Compose a release/copy setter"
  (if (not pointerp)
      (ma-objc-setter-not-pointer name type)
    (let ((pname (ma-objc-make-param-name name type)))
      (concat (ma-objc-make-setter-decl name type pointerp)
              "\n{\n" ma-objc-offset "if (" pname " != " name ") {\n"
              ma-objc-offset ma-objc-offset
              "[" name " release];\n"
              ma-objc-offset ma-objc-offset name " = [" pname " copy];"
              "\n" ma-objc-offset "}\n}"))))

(defun ma-objc-dealloc-ivar (name)
  (concat "[self set" (upcase-initials name) ":nil];"))

(defun ma-objc-dealloc (names)
  (concat "- (void) dealloc\n{\n" ma-objc-offset
          (mapconcat 'ma-objc-dealloc-ivar names (concat "\n" ma-objc-offset))
          "\n" ma-objc-offset "[super dealloc];\n}\n"))

(defun ma-objc-scan-ivars (txt)
  (let ((n (string-match %ma-objc-ivar-decl-regexp% txt 0))
        (res ()))
    (while n
      (setq res (cons (list (match-string 3 txt)
                            (match-string 1 txt)
                            (string= (match-string 2 txt) "*"))
                      res))
      (setq n (string-match %ma-objc-ivar-decl-regexp% txt (match-end 0))))
    (nreverse res)))

(defun ma-objc-scan-ivar-at-point ()
  (save-excursion
    (let ((begin (point)) (end 0))
      (end-of-line)
      (setq end (point))
      (beginning-of-line)
      (if (re-search-forward %ma-objc-ivar-decl-regexp% end t)
          (list (match-string 3)
                (match-string 1)
                (string= (match-string 2) "*"))
        nil))))

(defun ma-objc-insert-decls-for-ivars (ivars)
  "Insert setter and getters declarations for variables in `ivars'"
  (interactive)
  (mapc (lambda (ivar)
          (let ((name (ma-objc-ivar-name ivar))
                (type (ma-objc-ivar-type ivar))
                (pointerp (ma-objc-ivar-pointerp ivar)))
            (insert (ma-objc-getter-decl name type pointerp)
                    "\n"
                    (ma-objc-setter-decl name type pointerp)
                    "\n\n")))
        ivars))

(defun ma-objc-get-fun (spec prompt)
  (let* ((names (mapcar 'car spec))
         (prompt (concat prompt " (" (mapconcat 'identity names ", ") "): "))
         (sel nil))
    (while (or (not sel) (zerop (length sel)))
      (setq sel (completing-read prompt spec nil 1)))
        (cdr (assoc sel spec))))

(defconst %ma-objc-getters% '(("plain" . ma-objc-getter-simple)
                              ("copy" . ma-objc-getter-copy)))
(defconst %ma-objc-getters-np% '(("plain" . ma-objc-getter-simple)))

(defconst %ma-objc-setters% '(("retain/release" . ma-objc-setter-retain-release)
                              ("release/copy" . ma-objc-setter-release-copy)))
(defconst %ma-objc-setters-np% '(("plain" . ma-objc-setter-not-pointer)))

(defun ma-objc-insert-defs-for-ivars (ivars)
  "Insert setter and getters definitions for variables in `ivars'"
  (interactive)
  (mapc (lambda (ivar)
          (let* ((name (ma-objc-ivar-name ivar))
                 (type (ma-objc-ivar-type ivar))
                 (pointerp (ma-objc-ivar-pointerp ivar))
                 (gp (format "Getter style for '%s'" name))
                 (sp (format "Setter style for '%s'" name))
                 (getter (ma-objc-get-fun (if pointerp %ma-objc-getters%
                                            %ma-objc-getters-np%)
                                          gp))
                 (setter (ma-objc-get-fun (if pointerp %ma-objc-setters%
                                            %ma-objc-setters-np%)
                                          sp)))
            (when getter
              (insert (funcall getter name type pointerp) "\n\n"))
            (when setter
              (insert (funcall setter name type pointerp) "\n\n"))))
        ivars))

(defun ma-objc-insert-dealloc-for-ivars (ivars)
  "Insert dealloc definition for variables in `ivars'"
  (interactive)
  (let ((pvars
         (delete-if '(lambda (ivar) (not (ma-objc-ivar-pointerp ivar))) ivars)))
    (insert (ma-objc-dealloc (mapcar 'ma-objc-ivar-name pvars)))))

(defun ma-objc-find-ivar-decl (ivar)
  "Find the location of the given ivar declaration in current buffer"
  (goto-char (point-min))
  (if (not (re-search-forward (concat "^@interface +"
                                      (ma-objc-buffer-class)
                                      " *("
                                      ma-objc-private-category-name
                                      ")")
                              nil t))
      (progn
        (when (not (re-search-forward "^@implementation" nil t))
          (goto-char (point-max))
          (insert "\n\n"))
        (forward-line -1)
        (ma-objc-insert-private-category))
    (let ((begin (point)))
      (re-search-forward "^@end")
      (re-search-backward (ma-objc-make-getter-decl (ma-objc-ivar-name ivar)
                                                    (ma-objc-ivar-type ivar)
                                                    (ma-objc-ivar-pointerp
                                                     ivar))
                          begin t)))
  (beginning-of-line))

(defun ma-objc-find-ivar-def (ivar)
  "Find the location of the given ivar definition in current buffer"
  (goto-char (point-min))
  (when (not (re-search-forward "^@implementation " nil t))
    (goto-char (point-max))
    (insert "\n")
    (ma-objc-insert-implementation))
  (if (not (re-search-forward (ma-objc-make-getter-decl (ma-objc-ivar-name ivar)
                                                        (ma-objc-ivar-type ivar)
                                                        (ma-objc-ivar-pointerp
                                                         ivar))
                              nil t))
      (progn
        (re-search-forward
         (concat "^\\(\\(" (regexp-quote ma-objc-private-section-title)
                 "\\)\\|@end\\)"))
        (forward-line -1)))
  (beginning-of-line))


(defun ma-objc-do-create-class (class parent plist headers &optional desc nocom)
  "Create a new Objective-C class with name `class', deriving from `parent'
and implementing the list of protocols `plist' (a string list)"
  (let ((class-h (concat class ".h"))
        (class-m (concat class ".m"))
        (protocols (if plist (mapconcat 'identity plist ", ") nil)))
    (if (file-exists-p class-h) (error "%s exists!" class-h))
    (if (file-exists-p class-m) (error "%s exists!" class-m))
    (find-file class-h)
    (ma-objc-do-dox-file desc)
    (mapc '(lambda (h) (insert "#import " h "\n")) headers)
    (mapc '(lambda (f)
             (if (file-exists-p (concat f ".h"))
                 (insert "#import \"" f ".h\"\n")))
          (cons parent plist))
    (insert "\n@interface " class ": " parent
            (if (not (zerop (length protocols)))
                (concat "<" protocols ">")
              "")
            "\n{\n@private\n\n}\n\n\n@end")
    (unless nocom
      (re-search-backward "^@interface ")
      (ma-objc-dox-class))
    (find-file-other-window class-m)
    (ma-objc-dox-file)
    (insert "#import \"" class-h "\"\n\n\n")
    (ma-objc-insert-private-category)
    (goto-char (point-max))
    (ma-objc-insert-implementation)))


(provide 'ma-objc-utils)

;;; ma-objc-utils.el ends here
