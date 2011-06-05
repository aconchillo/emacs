;;; colors.el --- Setup Emacs color theme

(require 'color-theme)

(eval-after-load "color-theme"
  '(progn
     (color-theme-initialize)
     (color-theme-hober)))

;; User defined colors
(add-to-list 'load-path (expand-file-name "color-theme/" emacs-init-dir))

;(load "autumn")
;(color-theme-autumn)

(color-theme-bharadwaj)

;;; colors.el ends here
