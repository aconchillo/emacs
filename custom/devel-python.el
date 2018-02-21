;;; devel-python.el ---  Setup Python environment

(defun my-python-mode-hook ()
  (elpy-mode))

(add-hook 'python-mode-hook 'my-python-mode-hook)

;; Elpy
(setq elpy-rpc-python-command "python3")

;; Interactive shell
(setq python-shell-interpreter "python3")

;; Disable warning when launching the python3 interpreter
(setq python-shell-completion-native-enable nil)

;;; devel-python.el ends here
