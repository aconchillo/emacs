;;; devel.el --- Setup development environment

;; Find other file relations (ff-find-other-file)
(setq cc-other-file-alist
      (quote
       (("\\.cc$" (".hh" ".h"))
        ("\\.hh$" (".cc" ".C"))
        ("\\.c$" (".h"))
        ("\\.m$" (".h"))
        ("\\.h$" (".c" ".cc" ".C" ".CC" ".cxx" ".cpp" ".m"))
        ("\\.C$" (".H" ".hh" ".h"))
        ("\\.H$" (".C" ".CC"))
        ("\\.CC$" (".HH" ".H" ".hh" ".h"))
        ("\\.HH$" (".CC"))
        ("\\.cxx$" (".hh" ".h"))
        ("\\.cpp$" (".hh" ".h" ".hpp"))
        ("\\.hpp$" (".cpp")))))

;; Coding style
(setq c-default-style "gnu")

;; Margin
(add-to-list 'load-path (expand-file-name "margin-mode/" emacs-packages-dir))
(require 'margin)
(setq margin-column 78)
(add-hook 'c-mode-common-hook 'margin-mode)

;; Automatically scroll compilation buffer
(setq compilation-scroll-output t)
(require 'compile)

;; SCM
(add-to-list 'load-path (expand-file-name "xtla/" emacs-packages-dir))
(require 'xtla-autoloads)

;; Doxymacs
(add-to-list 'load-path (expand-file-name "doxymacs/no-autoconf/" emacs-packages-dir))
(require 'doxymacs)

(add-hook 'c-mode-common-hook 'doxymacs-mode)
(defun my-doxymacs-font-lock-hook ()
  (if (or (eq major-mode 'c-mode) (eq major-mode 'c++-mode))
      (doxymacs-font-lock)))
(add-hook 'font-lock-mode-hook 'my-doxymacs-font-lock-hook)

;;; devel.el ends here
