;;; devel-python.el ---  Setup Python environment

(require 'lsp-python)

(defun my-python-mode-hook ()
  (lsp-python-enable))

(add-hook 'python-mode-hook 'my-python-mode-hook)

;; Interactive shell
(setq python-shell-interpreter "python3")

;; Disable warning when launching the python3 interpreter
(setq python-shell-completion-native-enable nil)

;;; devel-python.el ends here
