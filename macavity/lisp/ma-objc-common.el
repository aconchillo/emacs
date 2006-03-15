;; ma-objc-common.el -- shared auxiliar functions for Objective-C
;; Start date: 12/03/05 (jao)
;; (c) MacAvity Software 2005

;;; Commentary:

;; This file contains shared auxiliar functions used by other packages
;; in this directory.

;;; Code:

;;; Customization:
(defgroup ma-objc nil
  "MacAvity's Objective-C suite"
  :prefix "ma-objc-")

(defcustom ma-objc-param-prefix "new"
  "The prefix to be added to the name of ivars in setter parameters"
  :group 'ma-objc
  :type 'string)

(defcustom ma-objc-private-category-name "PrivateAPI"
  "Name of the category used for private class messages"
  :group 'ma-objc
  :type 'string)

(defcustom ma-objc-private-section-title "//// Private API"
  "Title of the private section implementation"
  :group 'ma-objc
  :type 'string)

(defcustom ma-objc-default-method-body
  "[self doesNotRecognizeSelector:_cmd];\n"
  "Default new method body. Use `nil' for none."
  :group 'ma-objc
  :type 'string)

;;; Aux functions:

(defsubst ma-objc-buffer-class ()
  "Get name for this buffer's class"
  (file-name-nondirectory
   (file-name-sans-extension (buffer-file-name))))

(defun ma-objc-other-file (file)
  "Get the corresponding header/source file when `file' is a source/header file"
  (let ((base (file-name-sans-extension file))
        (ext (file-name-extension file)))
    (concat base (if (string= "h" ext) ".m" ".h"))))

(defsubst ma-objc-other-buffer ()
  "Call `ma-objc-other-file' with current's buffer filename"
  (ma-objc-other-file (buffer-file-name)))


(defun ma-objc-make-method-regexp (decl)
  "Create a method regexp from its declaration"
  (mapconcat 'regexp-quote (split-string decl) "[ \\n]*"))

(defun ma-objc-split-string (str &optional seps nilstr)
  (if (> emacs-major-version 20)
      (split-string str seps nilstr)
    (split-string str seps)))

(provide 'ma-objc-common)

;;; ma-objc-common.el ends here
