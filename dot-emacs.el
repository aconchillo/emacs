(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(org-agenda-files (quote ("~/Work/org/scew.org" "~/Work/org/work-ice.org"))))

;; Path variables
(defconst emacs-init-dir (expand-file-name "~/etc/emacs/")
  "User init directory")

(defconst init-lisp-dir (expand-file-name "custom/" emacs-init-dir)
  "Directory for the initialization files")

(defconst emacs-packages-dir (expand-file-name "elisp/" emacs-init-dir)
  "Directory for elisp packages")

;; Load init file if there
(defun init-load-file (part)
  (if (file-exists-p (concat init-lisp-dir part))
      (load (concat init-lisp-dir part))
    (if (file-exists-p (concat init-lisp-dir part ".el"))
         (load (concat init-lisp-dir part ".el"))
      (message (format "Loading %s (.el) ...failed" part)))))

;; Load paths (order matters)
(add-to-list 'load-path emacs-packages-dir)

;; Load modules
;(init-load-file "bbdb")           ;; BigBrother Database
(init-load-file "colors")         ;; Emacs color themes
(init-load-file "devel")          ;; General development
(init-load-file "edit")           ;; Edit/Typing customisations
;(init-load-file "emms")           ;; Emacs MultiMedia System
(init-load-file "erc")            ;; ERC IRC Client
;(init-load-file "eshell")         ;; Emacs Shell
(init-load-file "func")           ;; Lisp functions
(init-load-file "geiser")         ;; Scheme (geiser) initializations
(init-load-file "globals")        ;; Global initializations
;(init-load-file "gnus")           ;; Gnus News/Mail Client
;(init-load-file "jde")            ;; Java Development Environment
(init-load-file "keys")           ;; Key settings
;(init-load-file "lisp")           ;; LISP
(init-load-file "muse")           ;; Emacs Muse Mode
(init-load-file "octave")         ;; Octave
(init-load-file "org")            ;; Org
(init-load-file "sgml")           ;; SGML stuff
(init-load-file "skels")          ;; File skeletons
(init-load-file "tex")            ;; TeX initialization
(init-load-file "www")            ;; Web related stuff

;; color faces
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
)
