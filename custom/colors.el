;;; colors.el --- Setup Emacs color theme

;; Add custom themes path
(add-to-list 'custom-theme-load-path (expand-file-name "themes/" emacs-init-dir))

;; Treat pumba as a safe theme
(setq custom-safe-themes (quote ("85298aefde7415fbbee7a509e2ab30a6e4478130" default)))

;; Theme
(load-theme 'pumba)

;;; colors.el ends here
