;;; text.el --- Setup TeX environment

;; LaTeX
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq TeX-PDF-mode t)
(setq-default TeX-master nil)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)

;; NTE
(setq LaTeX-verbatim-regexp "\\(verbatim\\*?\\|cssample\\|csbadsample\\)")
(setq font-latex-verbatim-environments (quote ("verbatim" "verbatim*" "cssample" "csbadsample")))

;;; tex.el ends here
