;;; geiser.el ---  Setup Scheme environment

;; ParEdit
(add-to-list 'load-path (expand-file-name "paredit/" emacs-packages-dir))
(autoload 'paredit-mode "paredit"
  "Minor mode for pseudo-structurally editing Lisp code."
  t)

(add-hook 'scheme-mode-hook (lambda () (paredit-mode +1)))

;; Geiser
(load-file (expand-file-name "geiser/elisp/geiser.el" emacs-packages-dir))
(setq geiser-repl-history-filename "~/.emacs.d/geiser-history")

;; Automatically enable company mode for Scheme
(add-hook 'scheme-mode-hook (lambda () (company-mode +1)))

;;; geiser.el ends here
