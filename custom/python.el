;;; python.el ---  Setup Python environment

(require 'pymacs)
(pymacs-load "ropemacs" "rope-")

;; Automatically enable company mode for Python
(add-hook 'python-mode-hook (lambda () (company-mode +1)))

;;; python.el ends here
