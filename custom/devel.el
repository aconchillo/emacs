;;; devel.el --- Setup development environment

;; Find other file relations (ff-find-other-file)
(setq cc-other-file-alist
      (quote
       (("\\.cc$" (".hh" ".h"))
        ("\\.hh$" (".cc" ".C"))
        ("\\.c$" (".h"))
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

;; Ensuring tabs are spaces and using 2 spaces for offset
(setq indent-tabs-mode nil)
(setq c-basic-offset 2)

;; Automatically scroll compilation buffer
(require 'compile)
(setq compilation-scroll-output t)

;; Company mode "complete anything"
(add-to-list 'load-path (expand-file-name "company" emacs-packages-dir))
(autoload 'company-mode "company" nil t)

;; Doxymacs
(add-to-list 'load-path (expand-file-name "doxymacs/lisp/" emacs-packages-dir))
(require 'doxymacs)

(add-hook 'c-mode-common-hook 'doxymacs-mode)
(defun my-doxymacs-font-lock-hook ()
  (if (or (eq major-mode 'c-mode) (eq major-mode 'c++-mode))
      (doxymacs-font-lock)))
(add-hook 'font-lock-mode-hook 'my-doxymacs-font-lock-hook)

;;; devel.el ends here
