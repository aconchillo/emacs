;;; org.el ---  Setup Org environment

;; Indent tree
;(setq org-startup-indented t)
(setq org-hide-leading-stars t)

;; Adds CLOSED entry when TODO is DONE.
(setq org-log-done 'note)

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

(setq org-file-apps (quote ((auto-mode . emacs)
                            ("\\.x?html?\\'" . default)
                            ("\\.doc\\'" . default)
                            ("\\.odt\\'" . default)
                            ("\\.ods\\'" . default)
                            ("\\.pdf\\'" . default)
                            ("\\.xls\\'" . default))))

;;; org.el ends here
