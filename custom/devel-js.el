;;; devel-js.el ---  Setup Python environment

(add-to-list 'auto-mode-alist '("\\.vue\\'" . vue-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . js2-jsx-mode))
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-mode))

;; Set tab width to 2
(setq js-indent-level 2)
(setq typescript-indent-level 2)

;; Set css tab width to 2
(setq css-indent-offset 2)

;; JS/TS Language Server
(add-hook 'js2-mode-hook 'lsp)
(add-hook 'typescript-mode-hook 'lsp)

;;; devel-js.el ends here
