;;; colors.el --- Setup Emacs color theme

;; Add custom themes path
(add-to-list 'custom-theme-load-path (expand-file-name "themes/" emacs-init-dir))

;(load-theme 'pumba)

(load-theme 'solarized-dark t)

;; (custom-set-faces
;;  '(lsp-ui-doc-background ((t (:background "#19404a")))))

;;; colors.el ends here
