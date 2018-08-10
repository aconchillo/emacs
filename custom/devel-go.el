;;; devel-go.el --- Setup development environment for Go

(require 'lsp-go)

(defun my-go-mode-hook ()
  ;; Call Gofmt before saving
  (add-hook 'before-save-hook 'gofmt-before-save)
  ;; Customize compile command to run go build
  (if (not (string-match "go" compile-command))
      (set (make-local-variable 'compile-command)
           "go build -v && go test -v && go vet"))
  ;; Key bindings
  (local-set-key (kbd "M-.") 'xref-find-definitions)
  (local-set-key (kbd "M-,") 'xref-find-references)
  (local-set-key (kbd "M-*") 'xref-pop-marker-stack)
  ;; Completion
  (company-mode)
  (lsp-go-enable))

(add-hook 'go-mode-hook 'my-go-mode-hook)

;;; devel-go.el ends here
