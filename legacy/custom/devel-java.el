;;; devel-java.el --- Java Development Environment

(require 'lsp-java)

(require 'dap-java)

;; google-java-format
(add-to-list 'load-path (expand-file-name "google-java-format/" emacs-packages-dir))
(require 'google-java-format)

(setq google-java-format-executable "/usr/local/bin/google-java-format")

;; LSP settings
(add-hook 'java-mode-hook #'lsp)

(setq lsp-java-import-gradle-version "6.8.3")
(setq lsp-java-import-gradle-wrapper-enabled nil)
;; https://download.eclipse.org/jdtls/milestones/1.1.0/jdt-language-server-1.1.0-202104291503.tar.gz
;; https://download.eclipse.org/jdtls/milestones/1.2.0/jdt-language-server-1.2.0-202106301459.tar.gz
;; https://download.eclipse.org/jdtls/milestones/1.3.0/jdt-language-server-1.3.0-202108171748.tar.gz
;; https://download.eclipse.org/jdtls/milestones/1.4.0/jdt-language-server-1.4.0-202109161824.tar.gz
;; https://download.eclipse.org/jdtls/milestones/1.5.0/jdt-language-server-1.5.0-202110191539.tar.gz
;; https://download.eclipse.org/jdtls/milestones/1.7.0/jdt-language-server-1.7.0-202112161541.tar.gz
(setq lsp-java-jdt-download-url "https://download.eclipse.org/jdtls/milestones/1.8.0/jdt-language-server-1.8.0-202201261434.tar.gz")

;(setq lsp-java-save-actions-organize-imports t)
;(add-hook 'before-save-hook 'lsp-organize-imports)

;; This is so Lombok works, otherwise java can't find definitions.
(setq path-to-lombok (expand-file-name "~/.emacs.d/lsp-extras/lombok-1.18.12.jar"))

(add-to-list 'lsp-java-vmargs
             (concat "-javaagent:" path-to-lombok))
(add-to-list 'lsp-java-vmargs
             (concat "-Xbootclasspath/a:" path-to-lombok))

(add-to-list 'auto-insert-alist
             '((java-mode . "Java")
               nil
               "/*\n"
               " * This software is the confidential and proprietary information of SnapPays\n"
               " * Mobile, Inc. (d/b/a Papaya Payments), and may not be used, reproduced,\n"
               " * modified, distributed, publicly displayed or otherwise disclosed without\n"
               " * the express written consent of SnapPays Mobile, Inc.\n"
               " *\n"
               " * This software is a work of authorship by SnapPays Mobile, Inc. and\n"
               " * protected by the copyright laws of the United States and foreign\n"
               " * jurisdictions.\n"
               " *\n"
               " * Copyright (C) 2020+ SnapPays Mobile, Inc. All rights reserved.\n"
               " *\n"
               " */\n"))

;;; devel-java.el ends here
