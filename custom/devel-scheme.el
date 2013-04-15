;;; devel-scheme.el ---  Setup Scheme environment

;; General Scheme mode
(setq scheme-program-name "guile")

;; ParEdit
(add-hook 'scheme-mode-hook 'paredit-mode)

;; Geiser
(load-file "~/etc/emacs/elisp/geiser/elisp/geiser.el")

(setq geiser-repl-history-filename "~/.emacs.d/geiser-history")

;; ParEdit
(add-hook 'scheme-mode-hook 'paredit-mode)

;; Automatically enable company mode for Scheme
(add-hook 'scheme-mode-hook 'auto-complete-mode)

;;; devel-scheme.el ends here
