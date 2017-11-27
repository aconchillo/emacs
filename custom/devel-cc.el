;;; devel-cc.el --- Setup development environment for C/C++

;; Find other file relations (ff-find-other-file)
(setq cc-other-file-alist
      (quote
       (("\\.cc$" (".hh" ".h"))
        ("\\.hh$" (".cc" ".C"))
        ("\\.c$" (".h"))
        ("\\.h$" (".c" ".cc" ".C" ".CC" ".cxx" ".cpp" ".m"))
        ("\\.C$" (".h" ".hh" ".H"))
        ("\\.H$" (".C" ".CC"))
        ("\\.CC$" (".HH" ".H" ".hh" ".h"))
        ("\\.HH$" (".CC"))
        ("\\.cxx$" (".hh" ".h"))
        ("\\.cpp$" (".hh" ".h" ".hpp"))
        ("\\.hpp$" (".cpp")))))

;; Coding style
(setq c-default-style "gnu")

;; Ensure 2 spaces for offset
(setq c-basic-offset 2)

;; Colorful background if we reach limit
(add-hook 'c-mode-common-hook 'margin-mode)

;; Set tab width to 2
(add-hook 'c-mode-common-hook (lambda () (setq tab-width 2)))

;; Autocompletion
(add-hook 'c-mode-common-hook 'company-mode)

;; More setup
(defun my-c-mode-hook ()
  (local-set-key (kbd "M-.") 'rtags-find-symbol-at-point)
  (local-set-key (kbd "M-,") 'rtags-find-references-at-point)
  (local-set-key (kbd "M-[") 'rtags-location-stack-back)
  (local-set-key (kbd "M-]") 'rtags-location-stack-forward))
(add-hook 'c-mode-common-hook 'my-c-mode-hook)

;; devel.el ends here
