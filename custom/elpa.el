;;; packages.el --- Setup package management

;; Load packages first thing!
(if (version< emacs-version "27.0")
    (package-initialize))

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)

;; Fix HTTP1/1.1 problems
(setq url-http-attempt-keepalives nil)

;; Paradox GitHub token
(setq paradox-github-token t)
(setq paradox-automatically-star nil)

;;; packages.el ends here
