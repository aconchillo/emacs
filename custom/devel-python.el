;;; devel-python.el ---  Setup Python environment

(defun my-python-mode-hook ()
  (elpy-mode))

(add-hook 'python-mode-hook 'my-python-mode-hook)

;;; devel-python.el ends here
