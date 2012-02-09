;;; gnus.el --- Setup Gnus client

(require 'gnus)

;; Do not check new groups
(setq gnus-check-new-newsgroups nil)
(setq gnus-select-method '(nnnil ""))

;; Define how Gnus is to fetch news
(setq gnus-select-method
      '(nnimap "Mail"
	       (nnimap-address "localhost")
	       (nnimap-stream network)
	       (nnimap-authenticator login)))

;(setq nnimap-search-uids-not-since-is-evil nil)

;(setq nnmail-expiry-wait 'immediate)
;(setq nnmail-expiry-target "nnimap+gmail:[Gmail]/Trash")

;; SMTP (authentication is done in ~/.authinfo)
(setq message-send-mail-function 'smtpmail-send-it
      smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
      ;smtpmail-auth-credentials '(("smtp.gmail.com" 587 "login@gmail.com" nil))
      smtpmail-default-smtp-server "smtp.gmail.com"
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587)

;;; prefetch articles
(setq gnus-asynchronous t)
;;; prefetch as many articles as possible
(setq gnus-use-article-prefetch t)

(setq gnus-save-killed-list nil)

;; gnus agent
(setq gnus-agent t)
(setq mail-user-agent 'gnus-user-agent)

;; Group buffer stuff
(setq gnus-sum-thread-tree-indent " ")
(setq gnus-sum-thread-tree-root "| ")
(setq gnus-sum-thread-tree-false-root "| ")
(setq gnus-sum-thread-tree-single-indent "· ")
(setq gnus-sum-thread-tree-leaf-with-other "|-- ")
(setq gnus-sum-thread-tree-vertical "|")
(setq gnus-sum-thread-tree-single-leaf "`-- ")

(setq gnus-user-date-format-alist
      '(((gnus-seconds-today) . "Today, %H:%M")
        ((+ 86400 (gnus-seconds-today)) . "Yesterday, %H:%M")
        (604800 . "%A %H:%M") ;;that's one week
        ((gnus-seconds-month) . "%A %d")
        ((gnus-seconds-year) . "%B %d")
        (t . "%B %d '%y"))) ;;this one is used when no other does match

(setq gnus-group-line-format " %m%S%p%P:%~(pad-right 30)c %6y %B\n")
(setq gnus-topic-line-format "%i[ %(%{%n%}%) -- %A ]%v\n")

(setq gnus-group-uncollapsed-levels 2)
(add-hook 'gnus-select-group-hook 'gnus-group-set-timestamp)

;;; use topics
(add-hook 'gnus-group-mode-hook 'gnus-topic-mode)

;; thread stuff
;;; config
(setq
 ;; display threads in summary mode
 gnus-show-threads t
 ;; try to build threads by grabbing old headers
 gnus-fetch-old-headers nil
 ;; variable not documented
 gnus-gather-loose-threads t
 ;; list of functions for sorting threads
 ;; in the summary buffer
 gnus-thread-sort-functions
 ;; sort threads by root article number
 '(gnus-thread-sort-by-number
   ;; sort threads by root article score
   gnus-thread-sort-by-score)
 ;; variable not documented
 gnus-thread-hide-subjects nil
 ;; hide all threads initially
 gnus-thread-hide-subtree t)

(setq gnus-treat-display-smileys nil)
(setq gnus-treat-fill-long-lines nil)
(setq gnus-treat-fill-article nil)
(setq gnus-article-auto-eval-lisp-snippets nil)
(setq gnus-ignored-newsgroups
      "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\"]]\"[#'()]")

(setq gnus-gcc-mark-as-read t)

;; Define how Gnus is to read your mail.
;;; general params
; delete incoming files after mail is split
(setq mail-source-delete-incoming t)
(setq nnml-get-new-mail t)
; delete duplicates
(setq nnmail-treat-duplicates 'delete)

;;; say how Gnus is to store the mail: we use nnml groups.
(require 'nnir nil t)
(setq nnir-search-engine 'imap)

;;;; pgg: gpg mails
;; (require 'pgg)
;; ;; verify/decrypt only if mml knows about the protocl used
;; (setq mm-verify-option 'known)
;; (setq mm-decrypt-option 'known)

;; (setq mml2015-use 'pgg
;;       pgg-scheme 'gpg
;;       pgg-default-scheme 'gpg
;;       pgg-default-user-id "5343A3EC"
;;       pgg-cache-passphrase t
;;       pgg-passphrase-cache-expiry 100000)

;;; gnus.el ends here
