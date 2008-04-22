;;; bbdb.el --- Setup Insidious Big Brother Database

(require 'bbdb)

(require 'bbdb-hooks)

(require 'bbdb-autoloads)

(add-hook 'gnus-startup-hook 'bbdb-insinuate-gnus)
(add-hook 'message-setup-hook 'bbdb-define-all-aliases)

(bbdb-initialize 'gnus 'message)

(setq bbdb-offer-save 'always
      bbdb-quiet-about-name-mismatches t
      bbdb-use-alternate-names nil
      bbdb-notice-hook 'bbdb-auto-notes-hook
      bbdb-pop-up-target-lines 5
      bbdb-auto-notes-alist
      '(("Company" (".*" company 0 t))
        ("Organization" (".*" company 0 t))
        ("User-Agent" (".*" mailer 0 t))
        ("X-Mailer" (".*" mailer 0 t))))

;;; bbdb.el ends here
