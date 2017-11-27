;;; devel-go.el --- Setup development environment for Go

(defun my-go-mode-hook ()
  ;; Call Gofmt before saving
  (add-hook 'before-save-hook 'gofmt-before-save)
  ;; Customize compile command to run go build
  (if (not (string-match "go" compile-command))
      (set (make-local-variable 'compile-command)
           "go build -v && go test -v && go vet"))
  ;; Godef jump key binding
  (local-set-key (kbd "M-.") 'godef-jump)
  ;; Go back
  (local-set-key (kbd "M-[") 'pop-tag-mark)
  ;; Auto-completion
  (set (make-local-variable 'company-backends) '(company-go))
  (company-mode))

(add-hook 'go-mode-hook 'my-go-mode-hook)

;;; devel-go.el ends here
