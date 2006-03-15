;; ma-objc-custom.el -- Customisation for emacs builtins
;; Start date: 12/03/05 (jao)
;; (c) MacAvity Software 2005

;;; Commentary:
;;
;; This file provides default configurations for Objective-C,
;; customising standard emacs-cvs parameters.
;;

;;; Code:

;; tabs to spaces and tab width
(setq-default indent-tabs-mode nil)
(setq indent-tabs-width 4)

;; delete trailing whitespace
(when (< emacs-major-version 21)
  (defun delete-trailing-whitespace ()
    "Delete all the trailing whitespace across the current buffer.
All whitespace after the last non-whitespace character in a line is deleted.
This respects narrowing, created by \\[narrow-to-region] and friends.
A formfeed is not considered whitespace by this function."
    (interactive "*")
    (save-match-data
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward "\\s-$" nil t)
          (skip-syntax-backward "-" (save-excursion (forward-line 0) (point)))
          ;; Don't delete formfeeds, even if they are considered whitespace.
          (save-match-data
            (if (looking-at ".*\f")
                (goto-char (match-end 0))))
          (delete-region (point) (match-end 0)))))))

(add-hook 'write-file-hooks 'delete-trailing-whitespace)

;; automatic copyright date update
(add-hook 'write-file-hooks 'copyright-update)

;; compilation
(require 'compile)
(setq compilation-scroll-output t)
(setq compile-command "xcodebuild")
(setq c-default-style "bsd")

;; header/source extension pairing
(setq cc-other-file-alist
      (quote (("\\.cc$" (".hh" ".h"))
              ("\\.hh$" (".cc" ".C"))
              ("\\.c$" (".h"))
              ("\\.h$" (".m" ".c" ".cc" ".C" ".CC" ".cxx" ".cpp"))
              ("\\.m$" (".h"))
              ("\\.C$" (".H" ".hh" ".h"))
              ("\\.H$" (".C" ".CC"))
              ("\\.CC$" (".HH" ".H" ".hh" ".h"))
              ("\\.HH$" (".CC"))
              ("\\.cxx$" (".hh" ".h"))
              ("\\.cpp$" (".hpp")) ("\\.hpp$" (".cpp")))))

;; .h is obj-c
(add-to-list 'auto-mode-alist '("\\.h\\'" . objc-mode))

;; ensuring tabs are spaces and using 4 spaces for offset
(add-hook 'objc-mode-hook
          '(lambda ()
             (setq indent-tabs-mode nil)
             (setq c-basic-offset 4)))

(provide 'ma-objc-custom)

;;; ma-objc-custom.el ends here
