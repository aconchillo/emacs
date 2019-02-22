;;
;; Install package from command line. Example:
;;
;;   $ emacs --batch --expr "(define pkgs-to-install 'smex)" -l emacs-pkg-install.el
;;

(require 'package)

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)

;; Fix HTTP1/1.1 problems
(setq url-http-attempt-keepalives nil)

(package-initialize)

(package-refresh-contents)

(mapcar 'package-install pkgs-to-install)
