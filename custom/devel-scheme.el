;;; devel-scheme.el ---  Setup Scheme environment

;; ParEdit
(add-hook 'scheme-mode-hook 'paredit-mode)

;; Geiser
(setq geiser-repl-history-filename "~/.emacs.d/geiser-history")

;; Automatically enable company mode for Scheme
(add-hook 'scheme-mode-hook 'company-mode)

;;; devel-scheme.el ends here
