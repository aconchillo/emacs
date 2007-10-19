;;; jde.el --- Java Development Environment

(add-to-list 'load-path (expand-file-name "jde/lisp" emacs-packages-dir))

;; Defer JDE loading
(setq defer-loading-jde t)

(if defer-loading-jde
    (progn
      (autoload 'jde-mode "jde" "JDE mode." t)
      (setq auto-mode-alist
	    (append
	     '(("\\.java\\'" . jde-mode))
	     auto-mode-alist)))
  (require 'jde))

;; Configuration
(setq jde-ant-enable-find t)
(setq jde-ant-read-target t)
(setq jde-compiler (quote ("javac" "")))

;; Sets the basic indentation for Java source files to two spaces.
(defun my-jde-mode-hook ()
  (setq c-basic-offset 4))

(add-hook 'jde-mode-hook 'my-jde-mode-hook)

;; ELIB
(add-to-list 'load-path (expand-file-name "elib" emacs-packages-dir))

;; CEDET
(add-to-list 'load-path (expand-file-name "cedet/common" emacs-packages-dir))
(load-file (expand-file-name "cedet/common/cedet.el" emacs-packages-dir))

(setq semanticdb-default-save-directory (expand-file-name "~/.emacs.d/semantic"))

;;; jde.el ends here
