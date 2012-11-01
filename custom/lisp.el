;;; lisp.el --- Setup LISP environment

;; ParEdit
(mapc (lambda (mode)
	(let ((hook (intern (concat (symbol-name mode)
				    "-mode-hook"))))
	  (add-hook hook (lambda () (paredit-mode +1)))))
      '(emacs-lisp lisp inferior-lisp))

;; SLIME
(setq inferior-lisp-program "sbcl")
(require 'slime)
(slime-setup)

;;; lisp.el ends here
