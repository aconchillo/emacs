;;; colors.el --- Setup Emacs color theme

;; Add custom themes path
(add-to-list 'custom-theme-load-path (expand-file-name "themes/" emacs-init-dir))

;; Treat pumba as a safe theme
(setq custom-safe-themes (quote ("efd0a33925f986b31953377623e5bdd4129277e3e0227f47919c3512481a8fef" default)))

;; Theme
(load-theme 'pumba)

;;; colors.el ends here
