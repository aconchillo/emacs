;;
;; Install package from command line. Example:
;;
;;   $ emacs --batch --expr "(define pkgs-to-install 'smex)" -l emacs-pkg-install.el
;;

(require 'package)

(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

(add-to-list 'package-archives
             '("marmalade" . "https://marmalade-repo.org/packages/") t)

;; Disable security checks so we can download https repos with
;; self-signed certificate.
(setq network-security-level 'low)

;; Fix HTTP1/1.1 problems
(setq url-http-attempt-keepalives nil)

(package-initialize)

(package-refresh-contents)

(mapcar 'package-install pkgs-to-install)
