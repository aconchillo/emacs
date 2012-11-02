;;; python.el ---  Setup Python environment

;; Use python.el (https://github.com/fgallina/python.el)
(add-to-list 'load-path (expand-file-name "python.el/" emacs-packages-dir))
(require 'python)

(setq python-shell-interpreter "ipython")
(setq python-shell-interpreter-args "")
(setq python-shell-extra-pythonpaths '("/home/aleix/src/aleix/bitpacket/src"))
(setq python-shell-prompt-regexp "In \\[[0-9]+\\]: ")
(setq python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: ")
(setq python-shell-completion-setup-code
      "from IPython.core.completerlib import module_completion")
(setq python-shell-completion-module-string-code
      "';'.join(module_completion('''%s'''))\n")
(setq python-shell-completion-string-code
      "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")

(require 'pymacs)
(pymacs-load "ropemacs" "rope-")

;; Automatically find rope project
(setq ropemacs-guess-project 't)

;;; python.el ends here
