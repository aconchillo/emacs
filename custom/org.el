;;; org.el ---  Setup Org environment

(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

(eval-after-load "org-agenda"
  '(progn
     (define-prefix-command 'org-todo-state-map)

     (define-key org-mode-map "\C-cx" 'org-todo-state-map)

     (define-key org-todo-state-map "x"
       #'(lambda nil (interactive) (org-todo "CANCELLED")))
     (define-key org-todo-state-map "d"
       #'(lambda nil (interactive) (org-todo "DONE")))
     (define-key org-todo-state-map "f"
       #'(lambda nil (interactive) (org-todo "DEFERRED")))
     (define-key org-todo-state-map "l"
       #'(lambda nil (interactive) (org-todo "DELEGATED")))
     (define-key org-todo-state-map "s"
       #'(lambda nil (interactive) (org-todo "STARTED")))
     (define-key org-todo-state-map "w"
       #'(lambda nil (interactive) (org-todo "WAITING")))

     (define-key org-agenda-mode-map "\C-n" 'next-line)
     (define-key org-agenda-keymap "\C-n" 'next-line)
     (define-key org-agenda-mode-map "\C-p" 'previous-line)
     (define-key org-agenda-keymap "\C-p" 'previous-line)))

(define-key global-map [(control meta ?r)] 'org-capture)

(custom-set-variables
 '(org-agenda-files (quote ("~/src/org-mode/"
                            "~/src/org-mode/ice/"
                            "~/src/org-mode/oblong/")))
 '(org-agenda-file-regexp "^agenda-.*\\.org$")
 '(org-default-notes-file "~/src/org-mode/notes.org")
 '(org-agenda-ndays 7)
 '(org-deadline-warning-days 14)
 '(org-agenda-show-all-dates t)
 '(org-agenda-skip-deadline-if-done t)
 '(org-agenda-skip-scheduled-if-done t)
 '(org-agenda-start-on-weekday nil)
 '(org-reverse-note-order t)
 '(org-fast-tag-selection-single-key (quote expert))
 '(org-agenda-custom-commands
   (quote (("d" todo "DELEGATED" nil)
           ("c" todo "DONE|DEFERRED|CANCELLED" nil)
           ("w" todo "WAITING" nil)
           ("W" agenda "" ((org-agenda-ndays 21)))
           ("A" agenda ""
            ((org-agenda-skip-function
              (lambda nil
                (org-agenda-skip-entry-if (quote notregexp) "\\=.*\\[#A\\]")))
             (org-agenda-ndays 1)
             (org-agenda-overriding-header "Today's Priority #A tasks: ")))
           ("u" alltodo ""
            ((org-agenda-skip-function
              (lambda nil
                (org-agenda-skip-entry-if (quote scheduled) (quote deadline)
                                          (quote regexp) "\n]+>")))
             (org-agenda-overriding-header "Unscheduled TODO entries: "))))))
 '(org-capture-templates
   (quote (("n" "Notes" entry (file+headline "~/src/org-mode/notes.org" "Notes") "* %u %?")
           ("b" "BitPacket" entry (file+headline "~/src/org-mode/agenda-bitpacket.org" "Tasks")
            "* TODO %?\n  %u")
           ("g" "Guile" entry (file+headline "~/src/org-mode/agenda-guile.org" "Tasks")
            "* TODO %?\n  %u")
           ("p" "Personal" entry (file+headline "~/src/org-mode/agenda-personal.org" "Tasks")
            "* TODO %?\n  %u")
           ("o" "Oblong" entry (file+headline "~/src/org-mode/oblong/agenda-oblong.org" "Tasks")
            "* TODO %?\n  %u")))))

;; Agenda notifications
;; from http://emacs-fu.blogspot.com/2009/11/showing-pop-ups.html

;; warn 15 min in advance
(setq appt-message-warning-time 10)

;; show notificationnalso in modeline
(setq appt-display-mode-line t)

;; use our function to display notifications
(setq appt-display-format 'window)

;; active appt (appointment notification)
(appt-activate 1)

;; time display is required for this...
(display-time)

;; update appt each time agenda opened
(add-hook 'org-finalize-agenda-hook 'org-agenda-to-appt)

(defun djcb-popup (title msg &optional icon sound)
  "Show a popup if we're on X, or echo it otherwise; TITLE is the
title of the message, MSG is the context. Optionally, you can
provide an ICON and a sound to be played"
  (interactive)
  (when sound (shell-command
               (concat "mplayer -really-quiet " sound " 2> /dev/null")))
  (if (eq window-system 'x)
    (shell-command (concat "notify-send "
                           (if icon (concat "-i " icon) "")
                           " '" title "' '" msg "'"))
    ;; text only version
    (message (concat title ": " msg))))

;; our little façade-function for djcb-popup
(defun djcb-appt-display (min-to-app new-time msg)
  (djcb-popup (format "Appointment in %s minute(s)" min-to-app) msg
              "/usr/share/icons/gnome/32x32/status/appointment-soon.png"
              "/usr/share/sounds/ubuntu/stereo/message.ogg"))

(setq appt-disp-window-function (function acf-appt-display))

;;; org.el ends here
