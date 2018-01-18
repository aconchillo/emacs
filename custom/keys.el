;;; keys.el --- Setup key bindings

;; Keyboard input method
(setq default-input-method "catalan-prefix")

;; OS X
(setq mac-option-key-is-meta nil)
(setq mac-command-key-is-meta t)
(setq mac-command-modifier 'meta)
(setq mac-option-modifier nil)

;; Key bindings
(global-set-key (kbd "C-c o") 'ff-find-other-file)
(global-set-key (kbd "C-c c") 'compile)
(global-set-key (kbd "C-c r") 'recompile)
(global-set-key (kbd "C-c .") 'dabbrev-expand)
(global-set-key (kbd "M-i") 'indent-region)
(global-set-key (kbd "M-p") 'previous-error)
(global-set-key (kbd "M-n") 'next-error)

;; More key bindings
(global-set-key (kbd "M-g") 'goto-line)
(global-set-key (kbd "C-c d") 'insert-date)
(global-set-key (kbd "C-c e") 'fc-eval-and-replace)
(global-set-key (kbd "C-x C-b") 'electric-buffer-list)

(global-set-key (kbd "M-x") (lambda ()
                              (interactive)
                              (or (boundp 'smex-cache)
                                  (smex-initialize))
                              (global-set-key (kbd "M-x") 'smex)
                              (smex)))

;;; keys.el ends here
