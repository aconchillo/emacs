;;; devel-python.el ---  Setup Python environment

;; Setup LSP for python
(add-hook 'python-mode-hook 'lsp)

;; Interactive shell
(setq python-shell-interpreter "python3")

;; Disable warning when launching the python3 interpreter
(setq python-shell-completion-native-enable nil)

;;; devel-python.el ends here
