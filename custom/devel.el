;;; devel.el --- Setup development environment

;; Ensure tabs are spaces
(setq indent-tabs-mode nil)

;; Automatically scroll compilation buffer
(require 'compile)
(setq compilation-scroll-output t)

;; Magit
(setq magit-last-seen-setup-instructions "1.4.0")

;;; devel.el ends here
