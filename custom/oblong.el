;;; oblong.el ---  Oblong tools environment

(load-file "/home/aleix/src/tools/emacs/oblong.el")

(oblong-c-add-source-dir "/home/aleix/src/yovo/")

;; automatically insert oblong skels
(require 'autoinsert)
(setq auto-insert t)
(add-hook 'find-file-hooks 'auto-insert)
(setq auto-insert-directory "~/.autoinsert/")
(setq auto-insert-query t)

;; Tell geiser to use system guile with libPlasma environment.
;; It avoids having to "make install" yovo.
(setq geiser-guile-binary '("~/src/yovo/libPlasma/guile/env"
                            "/opt/oblong/deps-64-8/bin/guile"))

;;; oblong.el ends here
