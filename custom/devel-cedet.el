;;; devel-cedet.el --- Setup CEDET development environment

(load-file (expand-file-name "cedet/cedet-devel-load.el" emacs-packages-dir))

;; Semantic
(semantic-mode t)

(require 'semantic/ia)
(require 'semantic/bovine/gcc)

;; Display cursor position information
(global-semantic-idle-summary-mode t)

;(defun my-c-mode-cedet-hook ()
;  (local-set-key "." 'semantic-complete-self-insert)
;  (local-set-key ">" 'semantic-complete-self-insert))
;(add-hook 'c-mode-common-hook 'my-c-mode-cedet-hook)

;; Enable support for GNU global
(when (cedet-gnu-global-version-check t)
  (semanticdb-enable-gnu-global-databases 'c-mode)
  (semanticdb-enable-gnu-global-databases 'c++-mode))

;; Use projects
(global-ede-mode t)

;;; devel-cedet.el ends here
