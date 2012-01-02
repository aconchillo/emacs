;;; muse.el --- Setup Emacs Muse

(require 'muse-mode)

(require 'muse-html)
(require 'muse-project)

;; Autodetect muse files
(setq muse-file-extension nil muse-mode-auto-p t)
(add-hook 'find-file-hooks 'muse-mode-maybe)

;; Modify base styles
(muse-derive-style "personal-xhtml" "xhtml"
                   :header (expand-file-name "muse/aleix-header.html" init-lisp-dir)
                   :footer (expand-file-name "muse/aleix-footer.html" init-lisp-dir))
(muse-derive-style "project-xhtml" "xhtml"
                   :header (expand-file-name "muse/project-header.html" init-lisp-dir)
                   :footer (expand-file-name "muse/aleix-footer.html" init-lisp-dir))

(setq muse-project-alist
      '(("home"
         ("~/Documents/Wiki/personal"
          :default "home")
         (:base "personal-xhtml" :path "~/Work/www/personal"))
        ("bitpacket"
         ("~/Documents/Wiki/bitpacket"
          :default "index")
         (:base "project-xhtml" :path "~/Work/www/bitpacket"))
        ("dockland"
         ("~/Documents/Wiki/dockland"
          :default "index")
         (:base "project-xhtml" :path "~/Work/www/dockland"))
        ("mkprom-erc32"
         ("~/Documents/Wiki/mkprom-erc32"
          :default "index")
         (:base "project-xhtml" :path "~/Work/www/personal/mkprom-erc32"))
        ("playground"
         ("~/Documents/Wiki/playground"
          :default "index")
         (:base "project-xhtml" :path "~/Work/www/playground"))
        ("scew"
         ("~/Documents/Wiki/scew"
          :default "index")
         (:base "project-xhtml" :path "~/Work/www/scew"))
        ("weps"
         ("~/Documents/Wiki/weps"
          :default "index")
         (:base "project-xhtml" :path "~/Work/www/weps"))))

;; Specific functions and variables for HTML publishing

(defvar aleix/muse-fsf-link
  (concat "<a href=\"http://www.fsf.org/register_form?referrer=360\">"
          "<img src=\"./images/referrer.png\" alt=\"[FSF Associate Member]\" width=\"88\" height=\"31\"/>"
          "</a>"))

(defvar aleix/muse-analytics-alist
  '(("home" . "UA-2964502-1")
    ("bitpacket" . "UA-2964502-1")
    ("mkprom-erc32" . "UA-2964502-1")
    ("scew" . "UA-2964502-2")))

(defun aleix/muse-analytics (project)
  (cdr (assoc project aleix/muse-analytics-alist)))

(defun aleix/muse-analytics-html (project)
  (setq account (aleix/muse-analytics project))
  (if account
      (concat
       "<script type=\"text/javascript\">\n"
       "  var gaJsHost = ((\"https:\" == document.location.protocol) ? \"https://ssl.\" : \"http://www.\");\n"
       "  document.write(unescape(\"%3Cscript src='\" + gaJsHost + \"google-analytics.com/ga.js' type='text/javascript'%3E%3C/script%3E\"));\n"
       "</script>\n"
       "<script type=\"text/javascript\">\n"
       "  var pageTracker = _gat._getTracker(\"" account "\");\n"
       "  pageTracker._trackPageview();\n"
       "</script>\n")))

(defun aleix/muse-timestamp ()
  (let ((atts (and buffer-file-name (file-attributes buffer-file-name))))
    (concat
     "<span id=\"footdate\">Updated: "
     (format-time-string "%Y-%m-%d" (if atts (nth 5 atts) nil))
     "</span>")))

(defun aleix/muse-make-link (link text)
  (concat "<a href=\"" link "\">" text "</a>"))

(defun aleix/muse-index-link ()
  (aleix/muse-make-link "wikiindex.html" "Index"))

;;; muse.el ends here
