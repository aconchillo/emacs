;;; geiser.el ---  Setup Scheme environment

;; ParEdit
(add-to-list 'load-path (expand-file-name "paredit/" emacs-packages-dir))
(autoload 'paredit-mode "paredit"
  "Minor mode for pseudo-structurally editing Lisp code."
  t)

(add-hook 'scheme-mode-hook (lambda () (paredit-mode +1)))

;; Geiser
(load-file (expand-file-name "geiser/build/elisp/geiser.elc" emacs-packages-dir))

(require 'geiser-install)

;;; geiser.el ends here
