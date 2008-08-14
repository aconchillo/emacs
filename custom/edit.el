;;; edit.el --- Setup editing environment

;; Change dictionary program to GNU Aspell
(setq-default ispell-program-name "aspell")

;; Text editing
(setq default-major-mode 'text-mode)

;; Automatically use text mode unless stated otherwise
(add-hook 'text-mode-hook 'text-mode-hook-identify)
;; Automatically break lines
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;; Scroll only one line when move past bottom of screen
(setq scroll-step 1)

;; Fill column
(setq fill-column 72)

;; Kill whole line (kills return too)
(setq kill-whole-line t)

;; Convert tabs to spaces
(setq-default indent-tabs-mode nil)
(setq indent-tabs-width 4)

;; Delete trailing whitespaces
(add-hook 'write-file-hooks 'delete-trailing-whitespace)

;; Update copyright notice automagically
(add-hook 'write-file-hooks 'copyright-update)

;; Automatically reload files after they've been modified
(global-auto-revert-mode 1)

;;; edit.el ends here
