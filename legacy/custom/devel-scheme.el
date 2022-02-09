;;; devel-scheme.el ---  Setup Scheme environment

;; General Scheme mode
(setq scheme-program-name "guile")

;; Geiser
(setq geiser-repl-history-filename "~/.emacs.d/geiser-history")
(setq geiser-repl-current-project-function 'projectile-project-root)

;; ParEdit
(add-hook 'scheme-mode-hook 'paredit-mode)

;; Automatically enable company mode for Scheme
(add-hook 'scheme-mode-hook 'corfu-mode)

;;; devel-scheme.el ends here
