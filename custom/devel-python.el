;;; devel-python.el ---  Setup Python environment

(defun my-python-mode-hook ()
  ;; Auto-completion
  (set (make-local-variable 'company-backends) '(company-jedi))
  (company-mode))

(add-hook 'python-mode-hook 'my-python-mode-hook)

;;; devel-python.el ends here
