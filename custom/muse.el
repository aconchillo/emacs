;;; muse.el --- Setup Emacs Muse

(require 'muse-mode)

(require 'muse-html)
(require 'muse-project)

;; Autodetect muse files
(setq muse-file-extension nil muse-mode-auto-p t)
(add-hook 'find-file-hooks 'muse-mode-maybe)

;; Modify base styles
(muse-derive-style "my-xhtml" "xhtml"
                   :footer (expand-file-name "muse/aleix-footer.html"
                                             init-lisp-dir)
                   :style-sheet "<link rel=\"stylesheet\" type=\"text/css\" href=\"style/general.css\" />")

(setq muse-project-alist
      '(("web"
         ("~/Documents/muse/personal"
          :default "home")
         (:base "my-xhtml" :path "~/Work/www/personal"))
        ("bitpacket"
         ("~/Documents/muse/bitpacket"
          :default "index")
         (:base "my-xhtml" :path "~/Work/www/personal/BitPacket"))
        ("dockland"
         ("~/Documents/muse/dockland"
          :default "index")
         (:base "my-xhtml" :path "~/Work/www/dockland"))
        ("mkprom-erc32"
         ("~/Documents/muse/mkprom-erc32"
          :default "index")
         (:base "my-xhtml" :path "~/Work/www/personal/mkprom-erc32"))
        ("playground"
         ("~/Documents/muse/playground"
          :default "index")
         (:base "my-xhtml" :path "~/Work/www/playground"))
        ("scew"
         ("~/Documents/muse/scew"
          :default "index")
         (:base "my-xhtml" :path "~/Work/www/scew"))
        ("weps"
         ("~/Documents/muse/weps"
          :default "index")
         (:base "my-xhtml" :path "~/Work/www/weps"))))

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
       "<script src=\"http://www.google-analytics.com/urchin.js\" type=\"text/javascript\">\n"
       "  </script>\n"
       "  <script type=\"text/javascript\">\n"
       "    _uacct = \"" account "\";\n"
       "    urchinTracker();\n"
       "  </script>")))

(defun aleix/muse-timestamp ()
  (let ((atts (and buffer-file-name (file-attributes buffer-file-name))))
    (concat
     "<span class=\"footdate\">Updated: "
     (format-time-string "%Y-%m-%d" (if atts (nth 5 atts) nil)) "</span>")))

(defun aleix/muse-make-link (link text)
  (concat "<a href=\"" link "\">" text "</a>"))

(defun aleix/muse-index-link ()
  (aleix/muse-make-link "wikiindex.html" "Index"))

;;; muse.el ends here
