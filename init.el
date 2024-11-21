;;; init.el --- MarTango's .emacs -*- coding: utf-8; lexical-binding: t; byte-compile-warnings: (cl-functions); -*-

;; Description: MarTango's .emacs
;; URL: https://github.com/martango/.emacs.d/init.el

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;; This is my (Martin Tang) Emacs initialisation file.

;;; Code:

(require 'package)
(setq package-enable-at-startup nil
      package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("gnu" . "http://elpa.gnu.org/packages/")
                         ;; ("marmalade" . "https://marmalade-repo.org/packages/")
                         ;; ("org" . "http://orgmode.org/elpa/")
                         ))
(when (version< emacs-version "27")
  (package-initialize))
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
(require 'bind-key)

;; Keybindings
;; (dolist (folder '("lisp" "site-lisp"))
;;         (add-to-list 'load-path (concat user-emacs-directory folder)))
;; not needed since we use the :load-path keyword in use-package?
;; (add-to-list 'load-path (concat user-emacs-directory "lisp"))

(line-number-mode t)

(setq initial-major-mode 'org-mode)
(setq initial-scratch-message "")

;; EViL

(use-package evil :ensure t
  :custom
  (evil-want-integration t)
  (evil-want-keybinding nil)
  :bind (:map evil-insert-state-map
              ("C-;" . evil-normal-state))
  :init
  (evil-mode))

(use-package evil-collection :after evil :ensure t
  :config
  (evil-collection-init)
  :custom
  (evil-collection-magit-use-$-for-end-of-line nil)
  )

(use-package async :ensure t
  :config
  (setq async-bytecomp-allowed-packages '(all))
  (async-bytecomp-package-mode 1))

;; For GPG passphrase stuff
;; `brew install gnupg`
(defvar epg-pinentry-mode)
(setf epg-pinentry-mode 'loopback)

(setq custom-file (concat user-emacs-directory "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

(setq backup-directory-alist '(("." . "~/.emacs.d/backups/")))
(setq inhibit-startup-screen t)
(setq-default indent-tabs-mode nil)
(fset 'yes-or-no-p 'y-or-n-p)
(global-set-key (kbd "<s-up>") 'toggle-frame-fullscreen)
(show-paren-mode)
(menu-bar-mode -1)
(tool-bar-mode -1)

;;; Appearance

(use-package dracula-theme
  :ensure t
  :config (load-theme 'dracula t))


(use-package frame
  :no-require t
  :init
  (defun my/frame-behaviours (&optional frame)
    "Set appearance of FRAME depending on terminal or GUI."
    (with-selected-frame (or frame (selected-frame))
      (unless window-system
        (set-face-background 'default "nil" frame))))
  :hook (after-make-frame-functions . my/frame-behaviours))

;;; Packages

;; Emacs-Lisp
(use-package nameless
  :ensure t
  :defer)

;; HTML

(use-package web-mode
  :ensure t
  :defer t
  :custom
  (web-mode-code-indent-offset 2)
  (web-mode-markup-indent-offset 2)
  (web-mode-enable-auto-quoting nil)
  (css-indent-offset 2)
  ;; :mode "\\.html?\\'"
  ;; :mode "\\.tsx\\'"
  ;; :mode "\\.jsx\\'"
  ;; :mode "\\.js\\'"
  :hook
  (web-mode . (lambda () (flycheck-select-checker 'javascript-eslint))))

(with-no-warnings
  (use-package emmet-mode
    :ensure t
    :defer t
    :hook
    web-mode
    sgml-mode
    css-mode)
  )

;; JavaScript
(use-package add-node-modules-path
  :ensure t
  :hook
  js2-mode
  tide-mode
  web-mode)

;; (use-package js-ts-mode)

(use-package prettier-js
  :ensure t
  :disabled
  :custom
  (prettier-js-command "prettier")
  :hook
  (js2-mode . prettier-js-mode)
  (web-mode . prettier-js-mode)
  (typescript-mode . prettier-js-mode))

(use-package reformatter :ensure t
  :init
  (reformatter-define prettier-fmt :program "prettier" :group 'emacs)
  (reformatter-define gofmt-fmt :program "gofmt" :group 'go))

;; (use-package js2-mode
;;   :disabled
;;   :ensure t
;;   :mode "\\.js\\'"
;;   :interpreter ("node" . js2-mode)
;;   :custom (js2-basic-offset 2))

;; (use-package tide
;;   :disabled
;;   :ensure t
;;   :after flycheck
;;   :custom
;;   (typescript-indent-level 2)
;;   (company-tooltip-align-annotations t)
;;   (flycheck-check-syntax-automatically '(save mode-enabled))
;;   (tide-format-options
;;    (list :indentSize 2))
;;   :bind (:map tide-mode-map
;;               ("C-c C-r" . tide-rename-symbol)
;;               ("C-c r" . tide-refactor)
;;               ("C-M-i" . company-complete)
;;               ("M-r" . tide-references))
;;   :init
;;   (defun my/tide-hook ()
;;     (tide-setup)
;;     (tide-hl-identifier-mode)
;;     ;; (flycheck-select-checker 'typescript-tslint)
;;     (set (make-local-variable 'company-backends)
;;          '(company-tide company-files)))
;;   :config
;;   ;; (flycheck-add-mode 'tsx-tide 'web-mode)
;;   ;; (flycheck-add-mode 'typescript-tide 'web-mode)
;;   (flycheck-add-mode 'javascript-eslint 'web-mode)
;;   ;; (flycheck-add-mode 'typescript-tslint 'web-mode)
;;   (flycheck-add-next-checker 'javascript-tide '(warning . javascript-eslint) 'append)
;;   ;; (flycheck-add-next-checker 'typescript-tslint 'javascript-eslint 'append)
;;   (flycheck-add-next-checker 'tsx-tide 'javascript-eslint)
;;   :hook
;;   ;; (js2-mode . my/tide-hook)
;;   ;; (typescript-mode . my/tide-hook)
;;   ;; (web-mode . (lambda () (when (member (file-name-extension buffer-file-name) '("tsx" "jsx"))
;;   ;;                          (my/tide-hook))))
;;   )

;; Python
(defun my/flycheck-mypy--find-project-root (_checker)
  "Find setup.cfg in a parent directory of the current buffer."
  ;; This is a workaround for `https://gitlab.com/pycqa/flake8/issues/517'; see
  ;; also `https://github.com/flycheck/flycheck/issues/1722'
  (locate-dominating-file (or buffer-file-name default-directory) "mypy.ini"))

(use-package gv)

(use-package flycheck
  :ensure t
  :config
  (dolist (checker flycheck-checkers)
    (let ((modes (flycheck-checker-get checker 'modes)))
      (when (and (member 'go-mode modes) (not (member 'go-ts-mode modes)))
        (flycheck-add-mode checker 'go-ts-mode))))
  (global-flycheck-mode)
  :custom
  (flycheck-flake8rc ".flake8"))

(use-package flycheck-golangci-lint
  :ensure t
  :config (flycheck-add-mode 'golangci-lint 'go-ts-mode)
  :hook (go-ts-mode . flycheck-golangci-lint-setup))

(use-package anaconda-mode
  :ensure t
  :defer t
  :after
  (gv flycheck python)
  :custom
  (python-shell-interpreter "python")
  ;; (python-shell-completion-native-enable nil)
  :config
  (setf (flycheck-checker-get 'python-mypy 'working-directory) #'my/flycheck-mypy--find-project-root)
  (flycheck-add-next-checker 'python-pycompile '(warning . python-flake8))
  ;; (flycheck-add-next-checker 'python-pycompile 'python-mypy)
  ;; (flycheck-add-mode 'python)
  :hook
  python-ts-mode
  python-mode
  (python-mode . anaconda-eldoc-mode)
  (python-mode . (lambda ()
                   (add-to-list 'company-backends 'company-anaconda)
                   (flycheck-select-checker 'python-pycompile))))

(use-package company-anaconda
  :defer t
  :ensure t
  :after (company anaconda))

(add-hook 'before-save-hook #'delete-trailing-whitespace)

(use-package poetry :ensure t)

(use-package blacken
  :defer t
  :ensure t
  :hook
  (python-mode . blacken-mode))

(use-package python-isort
  :defer t
  :ensure t
  :hook (python-mode . python-isort-on-save-mode))

;; Other Modes

(use-package json-mode :ensure t :defer t :custom (js-indent-level 2))
(use-package csv-mode :ensure t :defer t)
(use-package markdown-mode :ensure t :defer t)
(use-package yaml-mode :ensure t :defer t)
(use-package graphql-mode :ensure t :defer t)

;; Org-Mode
(use-package dot-org :load-path "lisp")

;; Useful Tools
(use-package magit :ensure t :defer t :bind (("C-x g" . magit-status)))
(use-package forge
  :ensure t
  ;; :custom
  ;; See https://github.com/magit/ghub/issues/81
  ;; (ghub-use-workaround-for-emacs-bug 'force)
  ;; :config
  ;; (setq gnutls-log-level 1)
  )
;; (use-package magithub :disabled :ensure t :after magit :config (magithub-feature-autoinject t))
(use-package undo-tree :ensure t :init (global-undo-tree-mode t)
  :custom (undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo"))))

(use-package company
  :ensure t
  :custom
  (company-dabbrev-downcase nil)
  (company-dabbrev-ignore-case nil)
  :after evil
  :config
  (global-company-mode)
  (evil-define-key 'insert 'global-map
    (kbd "C-SPC") #'company-complete))

(use-package which-key :ensure t :init (which-key-mode 1))

(use-package counsel
  :ensure t
  :init
  (ivy-mode 1)
  :custom
  (magit-completing-read-function 'ivy-completing-read)
  :bind
  ("C-s" . swiper)
  ("M-x" . counsel-M-x)
  ("C-x C-f" . counsel-find-file)
  ("<f1> f" . counsel-describe-function)
  ("<f1> v" . counsel-describe-variable)
  ("<f1> l" . counsel-find-library)
  ("C-h i" . counsel-info-lookup-symbol))

(use-package projectile
  :ensure t
  :init (projectile-mode)
  :bind (:map projectile-mode-map
              ("C-c p" . projectile-command-map)))

(use-package counsel-projectile
  :after projectile
  :ensure t
  :init (counsel-projectile-mode))

(use-package ivy-hydra :defer t)

(use-package docker :ensure t)
(use-package kubernetes :ensure t :commands (kubernetes-overview))
(use-package kubernetes-evil :ensure t :after kubernetes)
(use-package kubel :ensure t :commands (kubel))
(use-package kubel-evil :ensure t :after kubel)

(use-package yasnippet :ensure t :init (use-package yasnippet-snippets :ensure t))
(use-package eldoc :config (global-eldoc-mode))
(use-package ace-window
  :ensure t
  :defer t
  :commands (ace-window)
  :bind
  ("M-i" . 'ace-window))

;; (use-package notmuch :custom (send-mail-function 'mailclient-send-it))
;; (use-package org-notmuch :after dot-org notmuch)

(use-package ansi-color
  :init
  (add-hook
   'compilation-filter-hook
   (lambda ()
     (ansi-color-apply-on-region compilation-filter-start (point)))))


;; (use-package pdf-tools) ;; (pdf-tools-install) with env vars set

(use-package go-ts-mode
  :custom
  (tab-width 2)
  (go-ts-mode-indent-offset 2)
  :hook
  (before-save . (lambda () (when (eq major-mode 'go-ts-mode) gofmt-fmt-buffer))))

(use-package gotest :ensure t)

(use-package treesit)

(use-package company-go :disabled :ensure t
  :hook go-ts-mode . (lambda ()
                    (add-to-list 'company-backends 'company-go)))

;; Rust

(use-package flycheck-rust :ensure t)
(use-package rust-mode :ensure t
  :after flycheck-rust
  :init
  (flycheck-add-mode 'rust-clippy 'rust-mode)
  (evil-define-key 'normal 'global-map (kbd "C-=")  #'eglot)
  :config
  (evil-define-key '(normal insert) rust-mode-map
     (kbd "C-c C-r") 'eglot-rename
     (kbd "C-h .") 'display-local-help)
  :hook
  (rust-mode . rust-enable-format-on-save)
  (rust-mode . flycheck-rust-setup))



(use-package cargo :ensure t)

(defun my/eglot-rust--find-project-root (dir)
  "Find location of Cargo.toml starting at DIR.

Eglot only uses vcs to find project roots by default"
  (when-let* ((output
               (let ((default-directory dir))
                 (shell-command-to-string "cargo metadata --no-deps --format-version 1")))
              (js (ignore-errors (json-read-from-string output)))
              (found (cdr (assq 'workspace_root js))))
    (cons 'transient  found)))

;; (defun my/project-find-go-module (dir)
;;   (when-let ((root (locate-dominating-file dir "go.mod")))
;;     (cons 'go-module root)))

;; (cl-defmethod project-root ((project (head go-module)))
;;   (cdr project))


(use-package eglot :ensure t
  :after flymake
  :hook
  (before-save . eglot-format-buffer)
  :custom
  (eglot-autoshutdown t)
  (eglot-extend-to-xref t)
  (next-error-function #'flymake-goto-next-error)
  :config
  (define-key eglot-mode-map [remap display-local-help] nil) ;; https://github.com/joaotavora/eglot/issues/454
  ;; (add-hook 'project-find-functions #'my/project-find-go-module)
  (define-key eglot-mode-map (kbd "C-c C-r") 'eglot-rename)
  (define-key eglot-mode-map (kbd "C-c r") 'eglot-rename)
  )

(use-package eglot-java
  :ensure t
  :hook java-ts-mode)

(use-package org-jira
  :disabled
  :ensure t
  :custom
  (jiralib-url "https://confluentinc.atlassian.net"))

(use-package server
  :no-require
  :hook (after-init . server-start))

(use-package so-long
  :config (global-so-long-mode))

(use-package mermaid-mode :disabled :ensure t)
(use-package ob-mermaid :disabled :ensure t)

(display-time-mode)

(use-package plantuml-mode :ensure t)
(use-package flycheck-plantuml :ensure t)

(use-package ob-plantuml
  :custom
  (org-plantuml-exec-mode 'plantuml))

(use-package gradle-mode :ensure t)
(use-package java-ts-mode :custom java-ts-mode-indent-offset 2)

(use-package eglot-with-flycheck
  :disabled
  :load-path "lisp"
  :config
  (flycheck-add-mode 'eglot 'web-mode)
  (flycheck-add-mode 'eglot 'java-mode)
  )

(use-package google-java-format
  :load-path "lisp"
  :defer
  :commands (google-java-format-buffer google-java-format google-java-format-region)
  :custom
  (google-java-format-executable "google-java-format"))


(use-package files :defer t :custom (require-final-newline t))

(unbind-key "s-t") ;; Mac os changing desktop is slow


(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

(use-package copilot
  :vc (:url "https://github.com/copilot-emacs/copilot.el"
            :rev :newest
            :branch "main")
  :config
  (defun my/copilot-tab-or-default ()
    (interactive)
    (if (and (bound-and-true-p copilot-mode)
             ;; Add any other conditions to check for active copilot suggestions if necessary
             )
        (copilot-accept-completion)
      (evil-insert 1)))
  (evil-define-key 'insert 'global (kbd "C-<tab>") 'my/copilot-tab-or-default)
  :hook prog-mode)

(put 'upcase-region 'disabled nil)

(use-package protobuf-ts-mode :ensure t)

(use-package flymake-mode
  :bind (("M-n" . flymake-goto-next-error)
         ("M-p" . flymake-goto-prev-error)))

(provide 'init)
;;; init.el ends here
