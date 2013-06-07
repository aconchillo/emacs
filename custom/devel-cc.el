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

;; Show a thin line in right margin
;; (add-hook 'c-mode-common-hook 'fci-mode)

;; Specify auto-complete sources
(defun my-ac-cc-mode ()
  (auto-complete-mode t)
  (add-to-list 'ac-sources 'ac-source-gtags)
  (add-to-list 'ac-sources 'ac-source-semantic))

(add-hook 'c-mode-common-hook 'my-ac-cc-mode)

;; Enable GNU Global mode. We use the newer version 3.7 provided by
;; global 6.2.8.
(add-to-list 'load-path (expand-file-name "gtags/" emacs-packages-dir))

(setq gtags-suggested-key-mapping t)
(setq gtags-disable-pushy-mouse-mapping t)

(require 'gtags)

(add-hook 'c-mode-common-hook 'gtags-mode)

;;; devel.el ends here
