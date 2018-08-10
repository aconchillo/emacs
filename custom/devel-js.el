;;; devel-js.el ---  Setup Python environment

(add-to-list 'auto-mode-alist '("\\.vue\\'" . vue-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . js2-jsx-mode))

;; Set tab width to 2
(setq js-indent-level 2)

;; Set css tab width to 2
(setq css-indent-offset 2)

;; JS/TS Language Server
(require 'lsp-javascript-typescript)
(add-hook 'js-mode-hook #'lsp-javascript-typescript-enable)
(add-hook 'typescript-mode-hook #'lsp-javascript-typescript-enable)

;;; devel-js.el ends here
