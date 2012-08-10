;;; oblong.el ---  Oblong tools environment

;(load-file "/home/aleix/src/tools/emacs/oblong.el")

(oblong-c-add-source-dir "/home/aleix/src/yovo/")
(oblong-c-add-source-dir "/home/aleix/src/streaming-appliance/")
(oblong-c-add-source-dir "/home/aleix/src/rtsp-viddle-server/")

;; automatically insert oblong skels
(require 'autoinsert)
(setq auto-insert t)
(add-hook 'find-file-hooks 'auto-insert)
(setq auto-insert-directory "~/.autoinsert/")
(setq auto-insert-query t)

;; use using directive
(setq oblong-c-skels-using-namespace-in-cpp t)

;; Tell geiser to use system guile with libPlasma environment.
;; It avoids having to "make install" yovo.
;(setq geiser-guile-binary '("~/src/yovo/libPlasma/guile/env" "guile"))
(setq geiser-guile-binary '("/opt/oblong/deps/bin/guile"))

;;; oblong.el ends here
