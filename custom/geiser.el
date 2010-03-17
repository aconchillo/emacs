;;; geiser.el ---  Setup Scheme environment

;; ParEdit
(add-to-list 'load-path (expand-file-name "paredit/" emacs-packages-dir))
(autoload 'paredit-mode "paredit"
  "Minor mode for pseudo-structurally editing Lisp code."
  t)

(add-hook 'scheme-mode-hook (lambda () (paredit-mode +1)))

;; Geiser
(load-file (expand-file-name "geiser/elisp/geiser.el" emacs-packages-dir))

(setq geiser-default-implementation 'guile)

;;; geiser.el ends here
