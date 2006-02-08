;;; edit.el --- Setup editing environment

;; Text editing
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;; Scroll only one line when move past bottom of screen
(setq scroll-step 1)

;; Fill column
(setq fill-column 72)

;; Kill whole line (kills return too)
(setq kill-whole-line t)
(add-hook 'write-file-hooks 'delete-trailing-whitespace)

;; Convert tabs to spaces
(setq indent-tabs-mode nil)
(setq indent-tabs-width 4)

;; Update copyright notice automagically
(add-hook 'write-file-hooks 'copyright-update)

;;; edit.el ends here
