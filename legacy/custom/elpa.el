;;; packages.el --- Setup package management

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)

(package-initialize)

;; Fix HTTP1/1.1 problems
(setq url-http-attempt-keepalives nil)

;; Paradox GitHub token
(setq paradox-github-token t)
(setq paradox-automatically-star nil)

;;; packages.el ends here
