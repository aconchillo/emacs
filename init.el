;;; init.el --- Emacs Configuration

(require 'package)

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)

(package-initialize)

(package-install 'use-package)

(require 'use-package)

(defconst emacs-init-dir (expand-file-name
                          (concat (getenv "HOME") "/src/emacs"))
  "User init directory")

(defconst init-lisp-dir (expand-file-name "custom/" emacs-init-dir)
  "Directory for the initialization files")

(defconst emacs-packages-dir (expand-file-name "elisp/" emacs-init-dir)
  "Directory for elisp packages")

(defun insert-date ()
  "Insert date at point."
  (interactive)
  (insert (format-time-string "%a %b %d, %Y %H:%M")))

(defun insert-uuid()
  "Insert UUID at point."
  (interactive)
  (insert (downcase (uuidgen-4))))

(defun fc-eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (prin1 (eval (read (current-kill 0)))
         (current-buffer)))

(defun my-colorized-log-buffer ()
  (cond
   ((eq major-mode 'compilation-mode)
    (ansi-color-apply-on-region compilation-filter-start (point-max)))
   ((eq major-mode 'dap-server-log-mode)
    (ansi-color-apply-on-region (point-min) (point-max)))))

(require 'project)

(defun my-project-try-cargo-toml (dir)
  "Try to locate a Rust project above DIR."
  (let ((found (locate-dominating-file dir "Cargo.toml")))
    (if (stringp found) `(transient . ,found) nil)))

(use-package emacs
  :bind (("C-c o" . ff-find-other-file)
         ("C-c c" . compile)
         ("C-c r" . recompile)
         ("C-c ." . dabbrev-expand)
         ("M-i" . indent-region)
         ("M-p" . previous-error)
         ("M-n" . next-error)
         ("C-0" . text-scale-adjust)
         ("C-+" . text-scale-increase)
         ("C--" . text-scale-decrease)
         ("C-c u" . insert-uuid)
         ("C-c d" . insert-date)
         ("C-c e" . fc-eval-and-replace)
         ("C-x C-b" . electric-buffer-list)
         ("<mouse-4>" . scroll-down-line)
         ("<mouse-5>" . scroll-up-line))
  :init
  ;; Auto insert
  (auto-insert-mode t)
  ;; Disable blinking cursor
  (blink-cursor-mode -1)
  ;; Column number
  (column-number-mode 1)
  ;; Automatically reload files after they've been modified
  (global-auto-revert-mode 1)
  ;; Enable syntax-highlighting.
  (global-font-lock-mode 1)
  ;; Disables menu bar
  (menu-bar-mode -1)
  ;; Show closing parenthesis
  (show-paren-mode 1)
  ;; Disables tool bar
  (tool-bar-mode -1)
  ;; Highlight line
  (global-hl-line-mode 1)
  :config
  ;; Initialize Emacs server
  (server-start)
  (setq user-full-name "Aleix Conchillo Flaqué")
  (setq user-mail-address "aconchillo@gmail.com")
  (setq auto-insert-query nil)
  ;; Disable sound
  (setq visible-bell t)
  ;; Scroll only one line when move past bottom of screen
  (setq scroll-step 1)
  (setq font-lock-maximum-size nil)
  ;; Add custom themes path
  ;;;;;(add-to-list 'custom-theme-load-path (expand-file-name "themes/" emacs-init-dir))
  ;; Colors
  (load-theme 'solarized-dark t)
  ;; Disable native compilation warnings
  (setq warning-suppress-types '((comp)))
  ;; Secure Remote Editing
  (setq tramp-default-method "scp")
  ;; Fix HTTP1/1.1 problems
  (setq url-http-attempt-keepalives nil)
  ;; Change dictionary program to GNU Aspell
  (setq-default ispell-program-name "aspell")

  (add-to-list 'backup-directory-alist
               `("." . ,(expand-file-name "~/.emacs.d/backups/")))

  ;; Text editing
  (setq default-major-mode 'text-mode)

  ;; Fill column
  (setq-default fill-column 80)

  ;; Kill whole line (kills return too)
  (setq kill-whole-line t)

  ;; Convert tabs to spaces
  (setq-default indent-tabs-mode nil)
  (setq indent-tabs-width 4)

  (setq compilation-scroll-output t)

  ;; Don't replace unix/dos endings
  (setq inhibit-eol-conversion t)

  ;; Keyboard input method
  (setq default-input-method "catalan-prefix")

  (pcase system-type
    ('darwin
     ;; (setq mac-option-key-is-meta nil)
     ;; (setq mac-command-key-is-meta t)
     ;; (setq mac-command-modifier 'meta)
     ;; (setq mac-option-modifier nil)
     (set-face-attribute 'default nil :family "MesloLGS NF")
     (set-face-attribute 'default nil :height 155))
    ('gnu/linux
     (set-face-attribute 'default nil :family "MesloLGS NF")
     (set-face-attribute 'default nil :height 130)))

  ;; Ensure tabs are spaces
  (setq indent-tabs-mode nil)

  ;; Settings for any frame
  (setq default-frame-alist
        '((vertical-scroll-bars . nil)
          (horizontal-scroll-bars . nil)
          (top . 35)
          (left . 50)
          (width . 135)
          (height . 42)))

  ;; Use objc-mode for objective-c files
  (add-to-list 'auto-mode-alist '("\\.h$" . objc-mode))
  (add-to-list 'auto-mode-alist '("\\.m$" . objc-mode))

  ;; Use objc-mode for objective-c++ files
  (add-to-list 'auto-mode-alist '("\\.mm$" . objc-mode))
  (add-to-list 'auto-mode-alist '("\\.pch$" . objc-mode))

  ;; Show ANSI colors on compilation buffers
  (add-hook 'compilation-filter-hook 'my-colorized-log-buffer)

  ;; Automatically use text mode unless stated otherwise
  (add-hook 'text-mode-hook 'text-mode-hook-identify)

  ;; Automatically break lines
  (add-hook 'text-mode-hook 'turn-on-auto-fill)

  ;; Delete trailing whitespaces
  (add-hook 'write-file-hooks 'delete-trailing-whitespace)

  ;; Update copyright notice automagically
  (add-hook 'write-file-hooks 'copyright-update)

  ;; Try to find the closest Cargo.toml to the file we are editing
  (add-hook 'project-find-functions 'my-project-try-cargo-toml)

  ;; Spell checking for text mode
  ;; (dolist (hook '(text-mode-hook))
  ;;   (add-hook hook (lambda () (flyspell-mode 1))))
  ;; (dolist (hook '(change-log-mode-hook log-edit-mode-hook))
  ;;   (add-hook hook (lambda () (flyspell-mode -1))))
  )

(use-package ansi-color
  :ensure t
  :hook ((shell-mode . ansi-color-for-comint-mode-on)
         (compilation-mode . ansi-color-for-comint-mode-on))
  :config
  (add-to-list 'comint-output-filter-functions 'ansi-color-process-output))

(use-package all-the-icons
  :ensure t
  :if (display-graphic-p))

(use-package all-the-icons-completion
  :ensure t
  :init
  (all-the-icons-completion-mode))

(use-package all-the-icons-dired
  :ensure t
  :hook (dired-mode . all-the-icons-dired-mode))

(defun blamer-callback-show-commit-diff (commit-info)
  (interactive)
  (let ((commit-hash (plist-get commit-info :commit-hash)))
    (when commit-hash
      (magit-show-commit commit-hash))))

(defun blamer-callback-open-remote (commit-info)
  (interactive)
  (let ((commit-hash (plist-get commit-info :commit-hash)))
    (when commit-hash
      (message commit-hash)
      (forge-browse-commit commit-hash))))

(use-package blamer
  :ensure t
  :bind (("C-c b" . global-blamer-mode)
         ("C-c i" . blamer-show-posframe-commit-info))
  :defer t
  :custom
  (blamer-idle-time 0.3)
  (blamer-min-offset 70)
  (blamer-smart-background-p nil)
  (blamer-bindings '(("<mouse-1>" . blamer-callback-open-remote)
                     ("<mouse-3>" . blamer-callback-show-commit-diff)))
  :custom-face
  (blamer-face ((t :foreground "#7a88cf"
                   :background nil
                   :height 110
                   :italic t))))

(use-package cmake-mode
  :ensure t
  :defer t)

(use-package consult
  :ensure t
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (;; C-c bindings (mode-specific-map)
         ("C-c h" . consult-history)
         ("C-c m" . consult-mode-command)
         ("C-c k" . consult-kmacro)
         ;; C-x bindings (ctl-x-map)
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ("<help> a" . consult-apropos)            ;; orig. apropos-command
         ;; M-g bindings (goto-map)
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings (search-map)
         ("M-s d" . consult-find)
         ("M-s D" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s m" . consult-multi-occur)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi))           ;; needed by consult-line to detect isearch
  )

(use-package corfu
  :ensure t
  :custom
  ;; Enable auto completion and configure quitting
  (corfu-auto t)
  (corfu-quit-no-match t)
  :init
  (global-corfu-mode))

(use-package dashboard
  :ensure t
  :init
  (setq dashboard-set-heading-icons t
        dashboard-set-file-icons t
        dashboard-projects-backend 'project-el
        dashboard-items '((recents  . 10)
                          (bookmarks . 5)
                          (projects . 5)
                          (agenda . 5)
                          (registers . 5)))
  :config
  (dashboard-setup-startup-hook))

(use-package direnv
  :ensure t
  :config
  (direnv-mode))

(use-package dockerfile-mode
  :ensure t
  :defer t)

(use-package forge
  :ensure t
  :after magit)

(use-package eglot
  :ensure t
  :defer t
  :hook ((c++-mode-hook . eglot-ensure)
         (python-mode . eglot-ensure))
  :bind (:map eglot-mode-map ("M-." . xref-find-definitions))
  :config
  (pcase system-type
    ('darwin
     (add-to-list 'eglot-server-programs
		  '(c++-mode "/opt/homebrew/opt/llvm@14/bin/clangd")))))

(use-package geiser
  :ensure t
  :defer t
  :config
  ;; General Scheme mode
  (setq scheme-program-name "guile")
  ;; Geiser
  (setq geiser-repl-history-filename "~/.emacs.d/geiser-history"))

(use-package geiser-guile
  :ensure t
  :after geiser)

(use-package graphql-mode
  :ensure t
  :defer t
  :mode "\\.graphqls\\'")

(use-package go-mode
  :ensure t
  :hook (go-mode . eglot-ensure)
  :mode "\\.go\\'")

(use-package go-rename
  :ensure t
  :after go)

(use-package js2-mode
  :ensure t
  :hook (js2-mode . eglot-ensure)
  :mode ("\\.js\\'" "\\.jsx\\'"))

(use-package kind-icon
  :ensure t
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default) ; to compute blended backgrounds correctly
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package marginalia
  :ensure t
  :init
  (marginalia-mode))

(use-package multiple-cursors
  :ensure t
  :bind (("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)))

(use-package ob-restclient
  :ensure t
  :defer t)

(use-package orderless
  :ensure t
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles . (partial-completion))))))

(use-package org
  :ensure t
  :bind (("\C-cl" . org-store-link)
         ("\C-ca" . org-agenda)
         :map global-map
         ([(control meta ?r)] . org-capture))
  :custom
  ((org-completion-use-ido t)
   (org-log-done 'time)
   (org-agenda-files (list "~/src/org/"))
   (org-agenda-file-regexp "^agenda-.*\\.org$")
   (org-default-notes-file "~/src/org/notes.org")
   (org-agenda-ndays 7)
   (org-deadline-warning-days 14)
   (org-agenda-show-all-dates t)
   (org-agenda-skip-deadline-if-done t)
   (org-agenda-skip-scheduled-if-done t)
   (org-agenda-start-on-weekday nil)
   (org-reverse-note-order t)
   (org-capture-templates
    '(("d" "Daily" entry (file+headline "~/src/org/agenda-daily.org" "Tasks") "* TODO %?\n  CREATED: %U"))))
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((shell . t))))

(use-package orglink
  :ensure t
  :hook (vterm-mode . orglink-mode))

(use-package osx-clipboard
  :ensure t
  :init
  (osx-clipboard-mode 1))

(use-package paradox
  :ensure t
  :defer t
  :config
  ;; Paradox GitHub token
  (setq paradox-github-token t)
  (setq paradox-automatically-star nil))

(use-package paredit
  :ensure t
  :hook
  ((emacs-lisp-mode . paredit-mode)
   (scheme-mode . paredit-mode)))

(use-package prettier
  :ensure t
  :defer t)

(use-package pyvenv-auto
  :ensure t
  :defer t
  :hook ((python-mode . pyvenv-auto-run)))

(use-package restclient
  :ensure t
  :defer t)

(use-package ripgrep
  :ensure t
  :defer t)

(use-package rustic
  :ensure t
  :hook (rust-mode . eglot-ensure)
  :config
  (setq rustic-lsp-client 'eglot)
  (setq rustic-format-on-save t))

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :ensure t
  :init
  (savehist-mode))

(use-package solarized-theme
  :ensure t)

(use-package treemacs
  :ensure t
  :defer t)

(use-package treemacs-magit
  :ensure t
  :after treemacs)

(use-package typescript-mode
  :ensure t
  :hook (typescript-mode . eglot-ensure)
  :mode ("\\.ts\\'" "\\.tsx\\'"))

;; Enable vertico
(use-package vertico
  :ensure t
  :init
  (vertico-mode)
  ;; Different scroll margin
  ;; (setq vertico-scroll-margin 0)

  ;; Show more candidates
  ;; (setq vertico-count 20)

  ;; Grow and shrink the Vertico minibuffer
  ;; (setq vertico-resize t)

  ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
  ;; (setq vertico-cycle t)
  )

(use-package vterm
  :ensure t
  :defer t
  :hook (vterm-mode . (lambda () (setq-local global-hl-line-mode nil)))
  :config
  (setq vterm-timer-delay 0.01))

(use-package yasnippet
  :ensure t
  :init
  (yas-global-mode 1))

(use-package async :ensure t :defer t)
(use-package docker-compose-mode :ensure t :defer t)
(use-package gitlab-ci-mode :ensure t :defer t)
(use-package json-mode :ensure t :defer t)
(use-package magit :ensure t :defer t)
(use-package markdown-mode :ensure t :defer t)
(use-package protobuf-mode :ensure t :defer t)
(use-package terraform-mode :ensure t :defer t)
(use-package yaml-mode :ensure t :defer t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
