;;; colors.el --- Setup Emacs color theme

;; Color themes
(add-to-list 'load-path (expand-file-name "color-theme/" emacs-packages-dir))

(require 'color-theme)

;; User defined colors
(add-to-list 'load-path (expand-file-name "color-theme/" emacs-init-dir))

(load "autumn")
(color-theme-autumn)

;;; colors.el ends here
