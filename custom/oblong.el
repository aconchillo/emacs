;;; oblong.el ---  Oblong tools environment

(oblong-c-add-source-dir "/home/aleix/src/yovo/")
(oblong-c-add-source-dir "/home/aleix/src/streaming-appliance/")
(oblong-c-add-source-dir "/home/aleix/src/rtsp-viddle-server/")
(oblong-c-add-source-dir "/home/aleix/src/gumbo/")

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
;(setq geiser-guile-binary '("/opt/oblong/deps/bin/guile"))
;(setq geiser-guile-binary '("~/src/yovo/libPlasma/guile/env"
;                            "/opt/oblong/deps-64-8/bin/guile"))

(defun update-yovo-comments ()
   "Updates /// style comments to /*."
   (interactive)
   (save-excursion
     (goto-char (point-min))
     (let ((start-comment t) (had-comment nil))
       (while (< (point) (point-max))
         (if (search-forward "///" (point-at-eol) t)
             (progn
               (replace-match " *" nil t)
               (setq had-comment t)
               (if start-comment
                   (progn
                     (beginning-of-line)
                     (insert "/**\n")
		    (forward-line -1)
		    (indent-for-tab-command)
		    (forward-line +1)
                     (setq start-comment nil))))
           (progn
             (setq start-comment t)
             (if had-comment
                 (progn
                   (insert " */\n")
                   (forward-line -1)
		  (indent-for-tab-command)
		  (forward-line +1)
                   (setq had-comment nil)))))
         (forward-line +1)))))

;;; oblong.el ends here
