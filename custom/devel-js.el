;;; devel-js.el ---  Setup Python environment

(add-to-list 'auto-mode-alist '("\\.vue\\'" . vue-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

;;(mmm-add-classes
;; '((vue-js
;;    :submode js2-mode
;;    :front "^<script>[\n\r]+"
;;    :back "^</script>$")))

;(add-to-list 'interpreter-mode-alist '("node" . js2-mode))

;;; devel-js.el ends here
