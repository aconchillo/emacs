;;; oblong.el ---  Oblong tools environment

;; Setup yobuild directory
(setq oblong-yobuild-dir "/opt/oblong/deps")

;; Oblong's C/C++ mode
(oblong-c-add-source-dir "/home/aleix/src/yovo/")
(oblong-c-add-source-dir "/home/aleix/src/mezzanine/")
(oblong-c-add-source-dir "/home/aleix/src/video/pool-rtsp-media-server/")
(oblong-c-add-source-dir "/home/aleix/src/video/rtsp-viddle-server/")
(oblong-c-add-source-dir "/home/aleix/src/video/streaming-appliance/")
(oblong-c-add-source-dir "/home/aleix/src/video/libFlow/")

;; Automatically insert oblong skels
(require 'autoinsert)
(setq auto-insert t)
(add-hook 'find-file-hooks 'auto-insert)
(setq auto-insert-directory "~/.autoinsert/")
(setq auto-insert-query t)

;; Use using directive
(setq oblong-c-skels-using-namespace-in-cpp t)

;; Tell geiser to use system guile with libPlasma environment.
;; It avoids having to "make install" yovo.
;(setq geiser-guile-binary '("~/src/yovo/libPlasma/guile/env"
;                            "/opt/oblong/deps/bin/guile"))

;; Update comments with Oblong style.
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
