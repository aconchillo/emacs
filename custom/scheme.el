;;; scheme.el --- Setup Scheme environment

;; ParEdit
(add-to-list 'load-path (expand-file-name "paredit/" emacs-packages-dir))
(autoload 'paredit-mode "paredit"
  "Minor mode for pseudo-structurally editing Lisp code."
  t)

(mapc (lambda (mode)
	(let ((hook (intern (concat (symbol-name mode)
				    "-mode-hook"))))
	  (add-hook hook (lambda () (paredit-mode +1)))))
      '(emacs-lisp lisp inferior-lisp))

;; Quack
(add-to-list 'load-path (expand-file-name "quack/" emacs-packages-dir))
(require 'quack)

;;; lisp.el ends here
