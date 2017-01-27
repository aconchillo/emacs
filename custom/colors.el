;;; colors.el --- Setup Emacs color theme

;; Add custom themes path
(add-to-list 'custom-theme-load-path (expand-file-name "themes/" emacs-init-dir))

;; Treat pumba as a safe theme
(setq custom-safe-themes (quote ("fa90d6964d1a8aabce590a3418a055b3360adba60055363077a2a906683df520"
                                 "4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328"
                                 default)))

;; Use a colarized terminal and don't load any theme for now.
;(load-theme 'pumba)
;(load-theme 'sanityinc-solarized-dark)

;;; colors.el ends here
