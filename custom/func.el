;;; func.el --- General Emacs functions

(defun insert-date ()
  "Insert date at point."
  (interactive)
  (insert (format-time-string "%a %b %d, %Y %H:%M")))

(defun fc-eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (prin1 (eval (read (current-kill 0)))
         (current-buffer)))

;;; func.el ends here
