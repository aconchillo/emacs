;;; www.el --- Setup WWW related environment

;; Choose browser
(setq gnus-button-url 'browse-url-generic)
(setq browse-url-generic-program "x-www-browser")
;(setq browse-url-generic-args (quote ("openURL")))
(setq browse-url-browser-function gnus-button-url)

;;; www.el ends here
