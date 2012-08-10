;;; devel.el --- Setup development environment

;; Find other file relations (ff-find-other-file)
(setq cc-other-file-alist
      (quote
       (("\\.cc$" (".hh" ".h"))
        ("\\.hh$" (".cc" ".C"))
        ("\\.c$" (".h"))
        ("\\.h$" (".c" ".cc" ".C" ".CC" ".cxx" ".cpp" ".m"))
        ("\\.C$" (".h" ".hh" ".H"))
        ("\\.H$" (".C" ".CC"))
        ("\\.CC$" (".HH" ".H" ".hh" ".h"))
        ("\\.HH$" (".CC"))
        ("\\.cxx$" (".hh" ".h"))
        ("\\.cpp$" (".hh" ".h" ".hpp"))
        ("\\.hpp$" (".cpp")))))

;; Coding style
(setq c-default-style "gnu")

;; Ensuring tabs are spaces and using 2 spaces for offset
(setq indent-tabs-mode nil)
(setq c-basic-offset 2)

;; Margin
(add-to-list 'load-path (expand-file-name "margin-mode/" emacs-packages-dir))
(require 'margin)
(setq margin-column 72)

;(define-globalized-minor-mode global-margin-mode margin-mode (lambda () (margin-mode t)))
;(global-margin-mode t)

(add-hook 'c-mode-common-hook 'margin-mode)
(add-hook 'emacs-lisp-mode-hook 'margin-mode)

;; Fill Column Indicator
(setq fci-style 'rule)
(setq fci-rule-width 1)

;(define-globalized-minor-mode global-fci-mode fci-mode (lambda () (fci-mode t)))
;(global-fci-mode t)

(add-hook 'c-mode-common-hook 'fci-mode)
(add-hook 'emacs-lisp-mode-hook 'fci-mode)

;; Automatically scroll compilation buffer
(require 'compile)
(setq compilation-scroll-output t)

;; Company mode "complete anything" (use autoload to load your backend)
(autoload 'company-mode "company" nil t)
(add-hook 'c-mode-common-hook 'company-mode)

;; yasnippet (already loaded in Debian)
;(require 'yasnippet)
;(yas/initialize)

;; Doxymacs (already loaded in Debian)
;(add-to-list 'load-path (expand-file-name "doxymacs/lisp/" emacs-packages-dir))
;(require 'doxymacs)

;(add-hook 'c-mode-common-hook 'doxymacs-mode)
;(defun my-doxymacs-font-lock-hook ()
;  (if (or (eq major-mode 'c-mode) (eq major-mode 'c++-mode))
;      (doxymacs-font-lock)))
;(add-hook 'font-lock-mode-hook 'my-doxymacs-font-lock-hook)

;;; devel.el ends here
