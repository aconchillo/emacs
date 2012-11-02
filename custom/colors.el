;;; colors.el --- Setup Emacs color theme

;; Add custom themes path
(add-to-list 'custom-theme-load-path (expand-file-name "themes/" emacs-init-dir))

;; Treat pumba as a safe theme
(setq custom-safe-themes (quote ("fa90d6964d1a8aabce590a3418a055b3360adba60055363077a2a906683df520" default)))

;; Theme
(load-theme 'pumba)

;;; colors.el ends here
