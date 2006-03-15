;; ma-objc-xc.el: Utility functions for working with Xcode
;; Start date: 13/03/05 (jao)
;; (c) MacAvity Software 2005

;;; Commentary:

;; This package defines useful functions for interacting with Xcode
;; and the project build process within emacs.

;; Public functions:
;;
;; - `ma-objc-xc-xcode' Invoke this command to switch to Xcode.
;; - `ma-objc-xc-ib' Invoke this command to switch to Interface Builder.
;; - `ma-objc-xc-projectrun' Invoke this command to run the project.
;; - `ma-objc-xc-run' Compile and, if successful, run the project.
;;

;;; TODO:
;;
;; * Wrapper for xcodebuild providing a list of projects and build styles
;;


;;; Code:

;; Public functions:

(defun ma-objc-xc-xcode ()
  "Switch to Xcode"
  (interactive)
  (start-process "xcode" nil "open" "-a" "Xcode"))

(defun ma-objc-xc-ib ()
  "Switch to IB"
  (interactive)
  (start-process "ib" nil "open" "-a" "Interface Builder"))

(defun ma-objc-xc-compilation-finish (buffer result)
  (when (string-match "finished" result)
    (switch-to-buffer %ma-objc-comp-buffer%)
    (ma-objc-xc-projectrun))
  (setq compilation-finish-function %ma-objc-prev-finish%))

(defvar %compile-cmd-history% '("xcodebuild"))

(defun ma-objc-xc-run ()
  "Build and run current app"
  (interactive)
  (let ((compile-cmd
         (read-string
          "Compile command (M-p for history; empty for no compilation): "
          (car %compile-cmd-history%) '(%compile-cmd-history% . 1))))
    (if compile-cmd
        (progn
          (setq %ma-objc-prev-finish% compilation-finish-function)
          (setq %ma-objc-comp-buffer% (current-buffer))
          (setq compilation-finish-function 'ma-objc-xc-compilation-finish)
          (compile compile-cmd))
      (ma-objc-xc-projectrun))))

;; This will probably change to (cd "build") ...
(defun ma-objc-xc-projectrun ()
  "Run current project"
  (interactive)
  (let* ((proj-dir (file-name-directory (buffer-file-name)))
         (app-name
          (file-name-nondirectory (substring proj-dir 0 -1)))
         (build-dir (expand-file-name "build" proj-dir))
         (app-dir
          (concat (expand-file-name app-name build-dir) ".app"))
         (app-exec (concat (expand-file-name "Contents/MacOS/" app-dir)
                           app-name))
         (buffname (concat "*" app-name "*")))
    (switch-to-buffer-other-window buffname)
    (delete-region (point-min) (point-max))
    (insert "Starting " app-name "\n-------------------------\n\n")
    (start-process app-name buffname app-exec)))
;;"open" "-a" app-dir)))

(provide 'ma-objc-xc)

;;; ma-objc-xc.el ends here
