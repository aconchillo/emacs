;;; devel.el --- Setup development environment

;; LSP for all languages
(require 'lsp)

;; Prefer Flycheck over Flymake
(setq lsp-prefer-flymake nil)

;; Don't show sidelines
(setq lsp-ui-sideline-enable nil)

;; Synchronize lsp-mode and treemacs
(setq lsp-treemacs-sync-mode 1)

;; Fixing a modeline.
;; See https://github.com/emacs-lsp/lsp-java/issues/276
(setq lsp-modeline-code-actions-segments '(count))

;; Recommended settings for LSP to work better.
;; See https://emacs-lsp.github.io/lsp-mode/page/performance/
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024))
(setq lsp-completion-provider :capf)

;; Auto insert
(auto-insert-mode t)
(setq auto-insert-query nil)

;; LSP file watch
(setq lsp-file-watch-threshold 3000)

(dolist (dir '(
                "[/\\\\]\\.gradle\\'"
                "[/\\\\]bin\\'"
                "[/\\\\]build\\'"
                "[/\\\\]gradle\\'"
                "[/\\\\]logs\\'"
                "[/\\\\]mongodb\\'"
                "[/\\\\]mysql\\'"))
  (push dir lsp-file-watch-ignored-directories))

;; Yasnippet
(yas-global-mode 1)

;; Ensure tabs are spaces
(setq indent-tabs-mode nil)

;; Automatically scroll compilation buffer
(require 'compile)
(setq compilation-scroll-output t)

;; Fix ANSI colors in compilation buffer (e.g. yarn)
(defun my-colorized-log-buffer ()
  (cond
   ((eq major-mode 'compilation-mode)
    (ansi-color-apply-on-region compilation-filter-start (point-max)))
   ((eq major-mode 'dap-server-log-mode)
    (ansi-color-apply-on-region (point-min) (point-max)))))
(add-hook 'compilation-filter-hook 'my-colorized-log-buffer)
(add-hook 'dap-server-log-mode-hook 'my-colorized-log-buffer)

;; Company
;(setq company-tooltip-align-annotations t)

;; Fix for https://github.com/tigersoldier/company-lsp/issues/79
;(setq company-lsp-cache-candidates t)

;; Some additional extensions.
(add-to-list 'auto-mode-alist '("\\.graphqls\\'" . graphql-mode))

;;; devel.el ends here
