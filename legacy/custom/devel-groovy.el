;;; devel-groovy.el --- Setup development environment for Groovy

(require 'lsp-groovy)

(add-hook 'groovy-mode-hook 'lsp)

(setq lsp-groovy-server-file (expand-file-name "~/.emacs.d/lsp-extras/groovy-language-server-all.jar"))

;;; devel-groovy.el ends here
