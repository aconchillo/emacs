;;; devel-octave.el ---  Setup Octave environment

;; Set octave for .m files
;; (setq auto-mode-alist
;;       (cons '("\\.m$" . octave-mode) auto-mode-alist))

(add-hook 'octave-mode-hook
          (lambda ()
            (abbrev-mode 1)
            (auto-fill-mode 1)))

;;; devel-octave.el ends here
