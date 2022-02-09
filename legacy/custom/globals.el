;;; globals.el --- Setup general environment

;; Initialize Emacs server
(server-start)

;; User variables
(setq user-full-name "Aleix Conchillo Flaque")
(setq user-mail-address "aconchillo@gmail.com")

;; Setup file backups directory
(add-to-list 'backup-directory-alist
             `("." . ,(expand-file-name "~/.emacs.d/backups/")))

;; Disable native compilation warnings
(setq warning-suppress-types '((comp)))

(dashboard-setup-startup-hook)

;; Battery
;(display-battery-mode 1)

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
(global-font-lock-mode 1)
(setq font-lock-maximum-size nil)

;; Copy to clipboard on macOS
(when (eq system-type 'darwin)

  ;; default Latin font (e.g. Consolas)
  (set-face-attribute 'default nil :family "MesloLGS NF")

  ;; default font size (point * 10)
  ;;
  ;; WARNING!  Depending on the default font,
  ;; if the size is not supported very well, the frame will be clipped
  ;; so that the beginning of the buffer may not be visible correctly.
  (set-face-attribute 'default nil :height 155)

  (osx-clipboard-mode +1))

;; Settings for any frame
(setq default-frame-alist
      '((vertical-scroll-bars . nil)
        (horizontal-scroll-bars . nil)
        (top . 35)
        (left . 20)
        (width . 135)
        (height . 42)))

;; vterm
(setq vterm-timer-delay 0.01)

;; selectrum
(selectrum-mode +1)
(selectrum-prescient-mode +1)
(prescient-persist-mode +1)
(setq prescient-filter-method '(literal regexp initialism))

;; Company mode in all buffers
(add-hook 'after-init-hook 'corfu-global-mode)

;; Projectile
(projectile-mode)

;; Ignore cquery files in projectile
(add-to-list 'projectile-globally-ignored-directories ".cquery_cached_index")

;; Global key binding for projectile
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

;; ANSI Colors
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(add-hook 'compilation-mode-hook 'ansi-color-for-comint-mode-on)
(add-to-list 'comint-output-filter-functions 'ansi-color-process-output)

;; Secure Remote Editing
(require 'tramp)
(setq tramp-default-method "scp")

;;; globals.el ends here
