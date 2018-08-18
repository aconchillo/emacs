;;; devel-scheme.el ---  Setup Scheme environment

;; General Scheme mode
(setq scheme-program-name "guile")

;; Geiser
(setq geiser-repl-history-filename "~/.emacs.d/geiser-history")

;; ParEdit
(add-hook 'scheme-mode-hook 'paredit-mode)

;; Automatically enable company mode for Scheme
(add-hook 'scheme-mode-hook 'company-mode)

;;; devel-scheme.el ends here
