;; config/ma-objc.el: .emacs code for ma-objc.el
;; Start date: 12/03/05 (jao)
;; (c) MacAvity Software 2005

;;; Put the directory containing ma-objc.el and require it
(add-to-list 'load-path (expand-file-name "~/Projects/MacAvity/Emacs/lisp"))
(require 'ma-objc)

;;; Customizable variables (values shown are the defaults)
(setq ma-objc-param-prefix "new")
(setq ma-objc-private-category-name "PrivateAPI")
(ma-objc-set-indent-offset 4)

;;; Keyboard shortcuts for objc-mode
(ma-objc-define-shortcuts)
;;; equivalent to:
;; (add-hook 'objc-mode-hook
;;           '(lambda ()
;;              (define-key objc-mode-map "\C-c\C-x" 'ma-objc-xc-xcode)
;;              (define-key objc-mode-map "\C-c\C-i" 'ma-objc-xc-ib)
;;              (define-key objc-mode-map "\C-cx" 'ma-objc-dox)
;;              (define-key objc-mode-map "\C-cl" 'ma-objc-dox-inline)
;;              (define-key objc-mode-map "\C-cg" 'ma-objc-dox-group)
;;              (define-key objc-mode-map "\C-cf" 'ma-objc-dox-file)
;;              (define-key objc-mode-map "\C-cp" 'ma-objc-insert-protocol-defs)
;;              (define-key objc-mode-map "\C-cv" 'ma-objc-insert-ivar)
;;              (define-key objc-mode-map "\C-cd" 'ma-objc-insert-dealloc)
;;              (define-key objc-mode-map "\C-cm" 'ma-objc-insert-method)))

