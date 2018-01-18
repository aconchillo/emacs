(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Path variables
(defconst emacs-init-dir (expand-file-name "~/src/emacs/")
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

;; We load CEDET before loading the built-in one.
;;(init-load-file "devel-cedet")    ;; CEDET development environment

;; Load all packages (built-in and installed)
(package-initialize)

;; General setup
(init-load-file "colors")         ;; Emacs color themes
(init-load-file "edit")           ;; Edit/Typing customisations
(init-load-file "elpa")           ;; Emacs Package Management
(init-load-file "func")           ;; Lisp functions
(init-load-file "globals")        ;; Global initializations
(init-load-file "keys")           ;; Key settings
(init-load-file "www")            ;; Web related stuff

;; Development
(init-load-file "devel")          ;; General development
(init-load-file "devel-cc")       ;; C/C++
(init-load-file "devel-js")       ;; Javascript
(init-load-file "devel-scheme")   ;; Scheme (geiser) initializations
(init-load-file "devel-go")       ;; Go
;(init-load-file "devel-haskell")  ;; Haskell
;(init-load-file "devel-java")     ;; Java Development Environment
;(init-load-file "devel-lisp")     ;; LISP
;(init-load-file "devel-octave")   ;; Octave
;(init-load-file "devel-python")   ;; Python
;(init-load-file "devel-skels")    ;; File skeletons
;(init-load-file "devel-sgml")     ;; SGML stuff

;; Specific modes
;(init-load-file "erc")            ;; ERC IRC Client
;(init-load-file "org")            ;; Org
;(init-load-file "tex")            ;; TeX initialization
;(init-load-file "bbdb")           ;; BigBrother Database
;(init-load-file "gnus")           ;; Gnus News/Mail Client
;(init-load-file "emms")           ;; Emacs MultiMedia System
;(init-load-file "eshell")         ;; Emacs Shell
;(init-load-file "muse")           ;; Emacs Muse Mode

;; color faces
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
