;;; devel-sgml.el --- Setup SGML environment

;; Don't complain when document does not have a doctype.
(setq sgml-warn-about-undefined-entities nil)

;; Not exactly related to editing html: enable editing help with
;; mouse-3 in all sgml files
(defun go-bind-markup-menu-to-mouse3 ()
  (define-key sgml-mode-map [(down-mouse-3)] 'sgml-tags-menu))

(add-hook 'sgml-mode-hook 'go-bind-markup-menu-to-mouse3)

;;; devel-sgml.el ends here
