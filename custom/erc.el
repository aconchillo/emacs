;;; erc.el --- IRC client setup

(require 'erc)

;; Enable services (e.g. NickServ)
(erc-services-mode 1)

;; User information
(setq erc-nick (quote ("aleix" "aleix_" "aleix__")))
(setq erc-user-full-name user-full-name)
(setq erc-email-userid "aleix")

;; Startup
(defmacro acf-erc-connect (command server port nick)
  "Create interactive command `command', for connecting to an IRC server. The
      command uses interactive mode if passed an argument."
  (fset command
        `(lambda (arg)
           (interactive "p")
           (if (not (= 1 arg))
               (erc-select :server ,server :port ,port :nick ,nick)
             (erc :server ,server
                  :port ,port
                  :nick ,nick
                  :full-name ,erc-user-full-name)))))

(acf-erc-connect erc-bitlbee "localhost" 6667 "aleix")
(acf-erc-connect erc-freenode "irc.freenode.net" 6667 "aleix")

;; When lines are wrapped (fill module needed)
(setq erc-fill-column fill-column)

;; Load password file
(load "~/.ercinfo")

;; Do not prompt for password
(setq erc-prompt-for-password nil)

;; New buffers
(setq erc-join-buffer (quote bury))

;; New private messages
(setq erc-auto-query (quote bury))

;; Interpret mIRC-style color commands in IRC chats (need irccolors
;; module).
(setq erc-interpret-mirc-color t)

;; ERC hook
(defun my-erc-mode-hook ()
  "Hook function to run when entering erc mode"
  (erc-add-scroll-to-bottom))

(add-hook 'erc-mode-hook 'my-erc-mode-hook)

;; ERC join hook
(defun bitlbee-identify ()
  "If we're on the bitlbee server, send the identify command to the
     #bitlbee channel."
  (when (and (string= "localhost" erc-session-server)
             (= 6667 erc-session-port)
             (string= "&bitlbee" (buffer-name)))
    (progn
      (erc-message "PRIVMSG" (format "%s set charset %s" (erc-default-target) "utf-8"))
      (erc-message "PRIVMSG" (format "%s identify %s" (erc-default-target) bitlbee-password)))))

(add-hook 'erc-join-hook 'bitlbee-identify)

;; Render html messages
(add-hook 'erc-insert-modify-hook 'maybe-wash-im-with-w3m)

(autoload 'w3m-region "w3m" "Render region using w3m")

(defun maybe-wash-im-with-w3m ()
  "Wash the current im with emacs-w3m."
  (save-restriction
    (with-current-buffer (current-buffer)
      (let ((case-fold-search t))
        (goto-char (point-min))
        (when (re-search-forward "<HTML>.*</HTML>" nil t)
          (print (match-string 0))
          (narrow-to-region (match-beginning 0) (match-end 0))
          (let ((w3m-safe-url-regexp mm-w3m-safe-url-regexp)
                w3m-force-redisplay)
            (w3m-region (point-min) (point-max))
            (goto-char (point-max))
            (delete-char -2))
          (when (and mm-inline-text-html-with-w3m-keymap
                     (boundp 'w3m-minor-mode-map)
                     w3m-minor-mode-map)
            (add-text-properties
             (point-min) (point-max)
             (list 'keymap w3m-minor-mode-map
                   ;; Put the mark meaning this part was rendered by emacs-w3m.
                   'mm-inline-text-html-with-w3m t))))))))

;; DCC settings

;; DCC Block size transfer
(setq erc-dcc-block-size 8192)

;; DCC directory
(setq erc-dcc-get-default-directory "~/Downloads")

;; DCC fixed available ports
(setq erc-dcc-port-range (quote (18000 . 18002)))

;; Initialises following ERC modules
(setq erc-modules
      '(autojoin completion fill irccontrols
                 netsplit notify stamp track))

(erc-update-modules)

;   (setq erc-modules
;         '(netsplit fill track pcomplete stamp
;                    completion ring button autojoin notify nickserv))


;; Timestamp
(setq erc-hide-timestamps nil
      erc-timestamp-only-if-changed-flag t
      erc-timestamp-format "[%H:%M] "
      erc-insert-timestamp-function 'erc-insert-timestamp-right)

;; NickServ login
(setq erc-prompt-for-nickserv-password nil)
(setq erc-nickserv-passwords
      `((freenode (("aleix" . ,freenode-password)))))

;; Autojoin channels
(setq erc-autojoin-channels-alist
      (quote
       (("freenode.net" "#geiser" "#gstreamer" "#guile"))))

;; Notify lists
(setq erc-notify-list (quote ("jao")))

;; Define ERC command (/NP) to display current song name
(defun erc-cmd-NP (&rest ignore)
  (erc-send-message (concat "NP: "
                            (emms-playlist-current))))

;;; erc.el ends here
