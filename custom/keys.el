;;; keys.el --- Setup key bindings

;; Keyboard input method
(setq default-input-method "catalan-prefix")

;; Key bindings
(global-set-key "\C-co" 'ff-find-other-file)
(global-set-key "\C-cs" 'speedbar-get-focus)
(global-set-key "\C-cc" 'compile)
(global-set-key "\C-cp" 'previous-error)
(global-set-key "\C-cn" 'next-error)
(global-set-key "\C-c." 'dabbrev-expand)
(global-set-key "\M-i" 'indent-region)

;; More key bindings
(global-set-key (kbd "M-g") 'goto-line)
(global-set-key (kbd "C-c d") 'insert-date)
(global-set-key (kbd "C-c e") 'fc-eval-and-replace)
(global-set-key (kbd "C-x C-b") 'electric-buffer-list)

;;; keys.el ends here
