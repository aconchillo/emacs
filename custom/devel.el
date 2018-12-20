;;; devel.el --- Setup development environment

;; LSP for all languages
(require 'lsp)
(require 'lsp-clients)

;; Ensure tabs are spaces
(setq indent-tabs-mode nil)

;; Automatically scroll compilation buffer
(require 'compile)
(setq compilation-scroll-output t)

;; Company
(setq company-tooltip-align-annotations t
      company-transformers nil)

;;; devel.el ends here
