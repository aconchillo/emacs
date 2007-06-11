(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(jde-ant-enable-find t)
 '(jde-ant-read-target t))

;; Path variables
(defconst emacs-init-dir  (expand-file-name "~/Library/Emacs/")
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
(init-load-file "colors")         ;; Emacs color themes
(init-load-file "devel")          ;; General development
(init-load-file "edit")           ;; Edit/Typing customisations
(init-load-file "func")           ;; Lisp functions
(init-load-file "globals")        ;; Global initializations
(init-load-file "keys")           ;; Key settings
;(init-load-file "jde")            ;; Java Development Environment
(init-load-file "lisp")            ;; LISP
(init-load-file "macavity")       ;; MacAvity
(init-load-file "muse")           ;; Emaacs Muse Mode
;(init-load-file "skels")          ;; File skeletons
(init-load-file "sgml")           ;; SGML stuff
(init-load-file "tex")            ;; TeX initialization

;; color faces
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )
