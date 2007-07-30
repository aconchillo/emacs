;;; globals.el --- Setup general environment

;; Initialize Emacs server
(server-start)

;; User variables
(setq user-full-name "Aleix Conchillo Flaque")
(setq user-mail-address "aleix@member.fsf.org")

;; Setup file backups directory
(add-to-list 'backup-directory-alist
             `("." . ,(expand-file-name "~/.emacs.d/backups/")))

;; Setup fink info directories
(require 'info)
(setq Info-directory-list (cons (expand-file-name "/sw/share/info") Info-directory-list))

;; Battery
(display-battery-mode 1)

;; Show time
;; (display-time)
(setq display-time-24hr-format t)

;; Column number
(column-number-mode 1)

;; Disables menu bar
(menu-bar-mode -1)

;; Disables tool bar
(if window-system
    (tool-bar-mode -1))

;; Disable blinking cursor
(blink-cursor-mode -1)

;; Disable sound
(setq visible-bell t)

;; Show closing parenthesis
(show-paren-mode 1)

;; Enable syntax-highlighting.
(require 'font-lock)
(global-font-lock-mode 1)
(setq font-lock-maximum-size nil)

;; Settings for any frame
(setq default-frame-alist
      '((top . 35)
        (left . 20)
        (width . 135)
        (height . 42)
	(background-color . "grey25")
	(background-mode . dark)
	(border-color . "Grey")
	(cursor-color . "Grey")
	(foreground-color . "beige")))

;; Speedbar
(setq speedbar-use-images nil)
(setq speedbar-frame-parameters
      (quote
       ((minibuffer)
	(width . 35)
	(border-width . 0)
	(menu-bar-lines . 0)
	(tool-bar-lines . 0)
	(unsplittable . t)
	(left-fringe . 0))))


;; Switch buffer improved
(require 'iswitchb)
(iswitchb-default-keybindings)

;; Mouse Wheel
(defun up-slightly () (interactive) (scroll-up 5))
(defun down-slightly () (interactive) (scroll-down 5))
(global-set-key [mouse-4] 'down-slightly)
(global-set-key [mouse-5] 'up-slightly)

(defun up-one () (interactive) (scroll-up 1))
(defun down-one () (interactive) (scroll-down 1))
(global-set-key [S-mouse-4] 'down-one)
(global-set-key [S-mouse-5] 'up-one)

(defun up-a-lot () (interactive) (scroll-up))
(defun down-a-lot () (interactive) (scroll-down))
(global-set-key [C-mouse-4] 'down-a-lot)
(global-set-key [C-mouse-5] 'up-a-lot)

;;; globals.el ends here
