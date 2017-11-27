;;; devel.el --- Setup development environment

;; Ensure tabs are spaces
(setq indent-tabs-mode nil)

;; Automatically scroll compilation buffer
(require 'compile)
(setq compilation-scroll-output t)

;; Margin
(add-to-list 'load-path (expand-file-name "margin-mode/" emacs-packages-dir))
(require 'margin)
(setq margin-column 72)

;; Enable auto complete. auto-complete-config has some extra sources
;; (e.g. semantic).
(require 'auto-complete)
(require 'auto-complete-config)

;; Magit
(setq magit-last-seen-setup-instructions "1.4.0")

;;; devel.el ends here
