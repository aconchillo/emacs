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

(use-package solarized-theme
  :ensure t)

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

  (add-hook 'compilation-filter-hook 'my-colorized-log-buffer)

;; Automatically use text mode unless stated otherwise
  (add-hook 'text-mode-hook 'text-mode-hook-identify)
  ;; Automatically break lines
  (add-hook 'text-mode-hook 'turn-on-auto-fill)

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

  ;; OS X
  (setq mac-option-key-is-meta nil)
  (setq mac-command-key-is-meta t)
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier nil)

  ;; Delete trailing whitespaces
  (add-hook 'write-file-hooks 'delete-trailing-whitespace)

  ;; Update copyright notice automagically
  (add-hook 'write-file-hooks 'copyright-update)

  (pcase system-type
    ('darwin
     (set-face-attribute 'default nil :family "Monaco")
     (set-face-attribute 'default nil :height 140))
    ('gnu/linux
     (set-face-attribute 'default nil :family "MesloLGS NF")
     (set-face-attribute 'default nil :height 130)))

  ;; Recommended settings for LSP to work better.
  ;; See https://emacs-lsp.github.io/lsp-mode/page/performance/
  (setq gc-cons-threshold 100000000)
  (setq read-process-output-max (* 1024 1024))

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

(use-package direnv
  :ensure t
  :config
  (direnv-mode))

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
  :config
  (setq consult-project-root-function #'projectile-project-root))

(use-package corfu
  :ensure t
  :custom
  ;; Enable auto completion and configure quitting
  (corfu-auto t)
  (corfu-quit-no-match t)
  :init
  (global-corfu-mode))

(use-package dap-mode
  :hook (dap-server-log-mode . my-colorized-log-buffer)
  :ensure t
  :after lsp-mode
  :init
  (dap-auto-configure-mode))

(use-package dap-java
  :defer t)

(use-package dashboard
  :ensure t
  :init
  (setq dashboard-set-heading-icons t
        dashboard-set-file-icons t
        dashboard-items '((recents  . 10)
                          (bookmarks . 5)
                          (projects . 5)
                          (agenda . 5)
                          (registers . 5)))
  :config
  (dashboard-setup-startup-hook))

(use-package geiser
  :ensure t
  :defer t
  :config
  ;; General Scheme mode
  (setq scheme-program-name "guile")
  ;; Geiser
  (setq geiser-repl-history-filename "~/.emacs.d/geiser-history")
  (setq geiser-repl-current-project-function 'projectile-project-root))

(use-package geiser-guile
  :ensure t
  :after geiser)

(use-package graphql-mode
  :ensure t
  :mode "\\.graphqls\\'")

(use-package go-mode
  :ensure t
  :mode "\\.go\\'")

(use-package go-rename
  :ensure t
  :after go)

(use-package flycheck
  :ensure t
  :after lsp)

(use-package js2-mode
  :ensure t
  :hook (js2-mode . lsp-deferred)
  :mode ("\\.js\\'" "\\.jsx\\'"))

(use-package kind-icon
  :ensure t
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default) ; to compute blended backgrounds correctly
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  :hook ((c-mode . lsp-deferred)
         (c++-mode . lsp-deferred))
  :bind ("M-*" . lsp-goto-implementation)
  :config
  ;; LSP file watch
  (setq lsp-file-watch-threshold 3000)
  ;; Prefer Flycheck over Flymake
  (setq lsp-prefer-flymake nil)
  ;; This is the default provider but let's set it anyways.
  (setq lsp-completion-provider :none)
  ;; Files and directories LSP should not watch.
  (dolist (dir '(
                 "[/\\\\]\\.gradle\\'"
                 "[/\\\\]bin\\'"
                 "[/\\\\]build\\'"
                 "[/\\\\]gradle\\'"
                 "[/\\\\]logs\\'"
                 "[/\\\\]mongodb\\'"
                 "[/\\\\]mysql\\'"))
  (push dir lsp-file-watch-ignored-directories)))

(use-package lsp-java
  :ensure t
  :hook ((java-mode . lsp-deferred)
         ;; (before-save lsp-organize-imports) ;; Only if google-java-format disabled
         )
  :bind (("C-c j b" . lsp-java-build-project)
         ("C-c j m" . dap-java-run-test-method)
         ("C-c j c" . dap-java-run-test-class))
  :config
  ;; Gradle version we use in most projects
  (setq lsp-java-import-gradle-version "6.8.3")
  (setq lsp-java-import-gradle-wrapper-enabled nil)
  ;; Eclipse JDT Language Server
  (setq lsp-java-jdt-download-url "https://download.eclipse.org/jdtls/milestones/1.9.0/jdt-language-server-1.9.0-202203031534.tar.gz")
  ;; This is so Lombok works, otherwise java can't find definitions.
  (setq path-to-lombok (expand-file-name "~/.emacs.d/lsp-extras/lombok-1.18.12.jar"))

  ;; Java VM
  (add-to-list 'lsp-java-vmargs
               (concat "-javaagent:" path-to-lombok))
  (add-to-list 'lsp-java-vmargs
               (concat "-Xbootclasspath/a:" path-to-lombok))

  ;; Fixing a modeline issue (only terminal).
  ;; See https://github.com/emacs-lsp/lsp-java/issues/276
  (unless (window-system)
    (setq lsp-modeline-code-actions-segments '(count)))

  ;; Format code automatically
  (setq google-java-format-executable "/usr/local/bin/google-java-format")
  (add-to-list 'load-path (expand-file-name "google-java-format/" emacs-packages-dir))
  (require 'google-java-format)

  ;; Only if google-java-format is disabled.
  ;; (setq lsp-java-save-actions-organize-imports t)
)

(use-package lsp-pyright
  :ensure t
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (lsp-deferred))))  ; or lsp

(use-package lsp-ui
  :ensure t
  :bind (([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
         ([remap xref-find-references] . lsp-ui-peek-find-references))
  :config
  ;; Don't show sidelines
  (setq lsp-ui-sideline-enable nil))

(use-package lsp-treemacs
  :ensure t
  :after treemacs lsp-mode
  :bind ("C-x t" . treemacs-select-window)
  :config
  ;; Synchronize lsp-mode and treemacs
  (setq lsp-treemacs-sync-mode 1))

(use-package marginalia
  :ensure t
  :init
  (marginalia-mode))

(use-package ob-restclient
  :ensure t)

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
  :ensure t)

(use-package projectile
  :ensure t
  :bind-keymap
  ;; Global key binding for projectile
  ("C-c p" . projectile-command-map)
  :init
  (projectile-mode)
  :config
  ;; Ignore cquery files in projectile
  (add-to-list 'projectile-globally-ignored-directories ".cquery_cached_index"))

(use-package restclient
  :ensure t)

(use-package ripgrep
  :ensure t)

(use-package rustic
  :ensure t)

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :ensure t
  :init
  (savehist-mode))

(use-package treemacs
  :ensure t
  :defer t)

(use-package treemacs-magit
  :ensure t
  :after treemacs)

(use-package treemacs-projectile
  :ensure t
  :after treemacs projectile)

(use-package typescript-mode
  :ensure t
  :hook (typescript-mode . lsp-deferred)
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
