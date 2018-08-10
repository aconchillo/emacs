;;; devel-cc.el --- Setup development environment for C/C++

;; Find other file relations (ff-find-other-file)
(setq cc-other-file-alist
      (quote
       (("\\.cc$" (".hh" ".h"))
        ("\\.hh$" (".cc" ".C"))
        ("\\.c$" (".h"))
        ("\\.h$" (".c" ".cc" ".C" ".CC" ".cxx" ".cpp" ".m"))
        ("\\.C$" (".h" ".hh" ".H"))
        ("\\.H$" (".C" ".CC"))
        ("\\.CC$" (".HH" ".H" ".hh" ".h"))
        ("\\.HH$" (".CC"))
        ("\\.cxx$" (".hh" ".h"))
        ("\\.cpp$" (".hh" ".h" ".hpp"))
        ("\\.hpp$" (".cpp")))))

;; Coding style
(setq c-default-style "gnu")

;; Ensure 2 spaces for offset
(setq c-basic-offset 2)

;; More setup
(require 'cquery)

(defun my-c-mode-hook ()
  ;; Key bindings
  (local-set-key (kbd "M-.") 'xref-find-definitions)
  (local-set-key (kbd "M-,") 'xref-find-references)
  (local-set-key (kbd "M-*") 'lsp-ui-peek-jump-forward)
  (local-set-key (kbd "M-]") 'lsp-ui-peek-jump-backward)
  ;; Completion
  (company-mode)
  (lsp-cquery-enable))
(add-hook 'c-mode-common-hook 'my-c-mode-hook)
(add-hook 'c++-mode-common-hook 'my-c-mode-hook)

;; Doxymacs
(add-to-list 'load-path (expand-file-name "doxymacs/lisp/" emacs-packages-dir))
(require 'doxymacs)

(add-hook 'c-mode-common-hook 'doxymacs-mode)

(defun my-doxymacs-font-lock-hook ()
  (if (or (eq major-mode 'c-mode) (eq major-mode 'c++-mode))
      (doxymacs-font-lock)))
(add-hook 'font-lock-mode-hook 'my-doxymacs-font-lock-hook)

;; devel.el ends here
