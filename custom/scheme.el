;;; scheme.el ---  Setup Scheme environment

;; ParEdit
(add-hook 'scheme-mode-hook (lambda () (paredit-mode +1)))

;; Geiser
(setq geiser-repl-history-filename "~/.emacs.d/geiser-history")

;; Automatically enable company mode for Scheme
(add-hook 'scheme-mode-hook (lambda () (company-mode +1)))

;;; geiser.el ends here
