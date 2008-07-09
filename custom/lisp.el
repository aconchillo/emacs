;;; lisp.el --- Setup LISP environment

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

;; SLIME
(setq inferior-lisp-program "/sw/bin/sbcl")
(require 'slime)
(slime-setup)

;;; lisp.el ends here
