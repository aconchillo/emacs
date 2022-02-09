;;; keys.el --- Setup key bindings

(require 'lsp-ui)
(require 'lsp-java)
(require 'dap-java)

;; Keyboard input method
(setq default-input-method "catalan-prefix")

;; OS X
(setq mac-option-key-is-meta nil)
(setq mac-command-key-is-meta t)
(setq mac-command-modifier 'meta)
(setq mac-option-modifier nil)

;; Key bindings
(global-set-key (kbd "C-c o") 'ff-find-other-file)
(global-set-key (kbd "C-c c") 'compile)
(global-set-key (kbd "C-c r") 'recompile)
(global-set-key (kbd "C-c .") 'dabbrev-expand)
(global-set-key (kbd "M-i") 'indent-region)
(global-set-key (kbd "M-p") 'previous-error)
(global-set-key (kbd "M-n") 'next-error)
(global-set-key (kbd "C-0") 'text-scale-adjust)
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

;; Development keys
(define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
(define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)
(define-key lsp-ui-mode-map (kbd "M-*")  #'lsp-goto-implementation)

;; Java
(define-key dap-mode-map (kbd "C-c j m") #'dap-java-run-test-method)
(define-key dap-mode-map (kbd "C-c j c") #'dap-java-run-test-class)
(define-key java-mode-map (kbd "C-c j b") #'lsp-java-build-project)

;; Treemacs
(global-set-key (kbd "C-x t") 'treemacs-select-window)

;; More key bindings
(global-set-key (kbd "M-g") 'consult-goto-line)
(global-set-key (kbd "C-c u") 'insert-uuid)
(global-set-key (kbd "C-c d") 'insert-date)
(global-set-key (kbd "C-c e") 'fc-eval-and-replace)
(global-set-key (kbd "C-x C-b") 'electric-buffer-list)

;; Enable mouse support
(unless window-system
  (global-set-key (kbd "<mouse-4>") 'scroll-down-line)
  (global-set-key (kbd "<mouse-5>") 'scroll-up-line))

;;; keys.el ends here
