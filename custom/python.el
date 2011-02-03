;;; python.el ---  Setup Python environment

(require 'pymacs)
(pymacs-load "ropemacs" "rope-")

;; Automatically enable company mode for Python
(add-hook 'python-mode-hook (lambda () (company-mode +1)))

;; Automatically find rope project
(setq ropemacs-guess-project 't)

;;; python.el ends here
