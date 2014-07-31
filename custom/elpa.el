;;; packages.el --- Setup package management

(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)

(add-to-list 'package-archives
             '("oblong" . "http://hacks-galore.org/aleix/emacs/packages/") t)

;; Fix HTTP1/1.1 problems
(setq url-http-attempt-keepalives nil)

;; Paradox GitHub token
(setq paradox-github-token "c4a9a92557755b14cf463998560ce55ebd7974d6")

;;; packages.el ends here
