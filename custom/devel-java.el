;;; devel-java.el --- Java Development Environment

(require 'lsp-java-boot)

(add-hook 'lsp-mode-hook #'lsp-lens-mode)
(add-hook 'java-mode-hook #'lsp-java-boot-lens-mode)
(add-hook 'java-mode-hook
          (lambda () (setq c-basic-offset 4)))

;;; devel-java.el ends here
