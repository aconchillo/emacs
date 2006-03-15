;; ma-objc.el -- All MacAvity Objective-c stuff
;; Start date: 12/03/05 (jao)
;; (c) MacAvity Software 2005

;;; Commentary:

;; This file requires all the additional MacAvity Obj-c packages. Just
;; do (require 'ma-objc) to get them. In addition, it defines the
;; following function:
;;
;; - `ma-objc-define-shortcuts' Add an Objc-mode hook that defines useful
;;   shortcuts for some useful functions defined in MacAvity's Objective-C
;;   suite.
;;

;;; Code:

(require 'ma-objc-custom)
(require 'ma-objc-utils)
(require 'ma-objc-dox)
(require 'ma-objc-xc)

(defun ma-objc-define-shortcuts ()
  (add-hook 'objc-mode-hook
            '(lambda ()
               (define-key objc-mode-map "\C-c\C-x" 'ma-objc-xc-xcode)
               (define-key objc-mode-map "\C-c\C-i" 'ma-objc-xc-ib)
               (define-key objc-mode-map "\C-c\C-r" 'ma-objc-xc-run)
               (define-key objc-mode-map "\C-cx" 'ma-objc-dox)
               (define-key objc-mode-map "\C-cl" 'ma-objc-dox-inline)
               (define-key objc-mode-map "\C-cg" 'ma-objc-dox-group)
               (define-key objc-mode-map "\C-cf" 'ma-objc-dox-file)
               (define-key objc-mode-map "\C-cC" 'ma-objc-create-class)
               (define-key objc-mode-map "\C-cT" 'ma-objc-create-uktest)
               (define-key objc-mode-map "\C-ct" 'ma-objc-insert-uktest)
               (define-key objc-mode-map "\C-cp" 'ma-objc-insert-protocol-defs)
               (define-key objc-mode-map "\C-cv" 'ma-objc-insert-ivar)
               (define-key objc-mode-map "\C-cd" 'ma-objc-insert-dealloc)
               (define-key objc-mode-map "\C-cm" 'ma-objc-insert-method))))


(provide 'ma-objc)

;;; ma-objc.el ends here
