;;; devel-js.el ---  Setup Python environment

(add-to-list 'auto-mode-alist '("\\.vue\\'" . vue-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . js2-mode))

;; Set tab width to 2
(setq js-indent-level 2)

;; Set css tab width to 2
(setq css-indent-offset 2)

;; TypeScript (tide)
(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  (company-mode +1)
  (setq typescript-indent-level 2))

;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)

;; formats the buffer before saving
;;(add-hook 'before-save-hook 'tide-format-before-save)

(add-hook 'typescript-mode-hook #'setup-tide-mode)

;; Vue
;;(mmm-add-classes
;; '((vue-js
;;    :submode js2-mode
;;    :front "^<script>[\n\r]+"
;;    :back "^</script>$")))

;(add-to-list 'interpreter-mode-alist '("node" . js2-mode))

;;; devel-js.el ends here
