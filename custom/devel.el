;;; devel.el --- Setup development environment

;; LSP for all languages
(require 'lsp)
(require 'lsp-clients)

;; Prefer Flycheck over Flymake
(setq lsp-prefer-flymake nil)

;; Don't show sidelines
(setq lsp-ui-sideline-enable nil)

;; Ensure tabs are spaces
(setq indent-tabs-mode nil)

;; Automatically scroll compilation buffer
(require 'compile)
(setq compilation-scroll-output t)

;; Company
(setq company-tooltip-align-annotations t)

;; Fix for https://github.com/tigersoldier/company-lsp/issues/79
(setq company-lsp-cache-candidates t)

;;; devel.el ends here
