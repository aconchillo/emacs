;;; macavity.el --- Setup MacAvity development environment

;;; Put the directory containing ma-objc.el and require it
(add-to-list 'load-path (expand-file-name "~/Projects/MacAvity/Emacs/lisp"))

(require 'ma-objc)

;;; Customizable variables (values shown are the defaults)
(setq ma-objc-param-prefix "new")
(setq ma-objc-private-category-name "PrivateAPI")
(ma-objc-set-indent-offset 4)

;;; Keyboard shortcuts for objc-mode
(ma-objc-define-shortcuts)

;;; macavity.el ends here
