;;; muse.el --- Setup Emacs Muse

(require 'muse-mode)

(require 'muse-html)
(require 'muse-project)

;; Autodetect muse files
(setq muse-file-extension nil muse-mode-auto-p t)
(add-hook 'find-file-hooks 'muse-mode-maybe)

;; Modify base styles
(muse-derive-style "my-xhtml" "xhtml"
                   :footer (expand-file-name "muse/muse-footer-web.html"
                                             init-lisp-dir))

(setq muse-project-alist
      '(("web"
         ("~/Documents/Wiki/personal"
          :default "home")
         (:base "my-xhtml" :path "~/Sites"))
        ("bitpacket"
         ("~/Documents/Wiki/bitpacket"
          :default "index")
         (:base "my-xhtml" :path "~/Sites/BitPacket"))
        ("dockland"
         ("~/Documents/Wiki/dockland"
          :default "index")
         (:base "my-xhtml" :path "~/Work/www/dockland"))
        ("playground"
         ("~/Documents/Wiki/playground"
          :default "index")
         (:base "my-xhtml" :path "~/Work/www/playground"))
        ("scew"
         ("~/Documents/Wiki/scew"
          :default "index")
         (:base "my-xhtml" :path "~/Work/www/scew"))
        ("weps"
         ("~/Documents/Wiki/weps"
          :default "index")
         (:base "my-xhtml" :path "~/Work/www/weps"))))

;; HTML publishing

(setq muse-xhtml-style-sheet
      "<link rel=\"stylesheet\" type=\"text/css\" href=\"style/general.css\" />")

(defvar aleix/muse-fsf-link
  (concat "<a href=\"http://www.fsf.org/register_form?referrer=360\">"
          "<img src=\"./images/referrer.png\" alt=\"[FSF Associate Member]\" width=\"88\" height=\"31\"/>"
          "</a>"))

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
