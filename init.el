;;; init.el --- MarTango's .emacs -*- coding: utf-8; lexical-binding: t; byte-compile-warnings: (not free-vars noruntime); -*-

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
                         ("marmalade" . "https://marmalade-repo.org/packages/")
                         ("gnu" . "http://elpa.gnu.org/packages/")
                         ("org" . "http://orgmode.org/elpa/")))
(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
(require 'bind-key)

;; Keybindings
(mapc (lambda (folder)
        (add-to-list 'load-path (concat user-emacs-directory folder)))
      '("lisp" "site-lisp"))

(line-number-mode t)

;; EViL

(use-package evil :ensure t
  :custom
  (evil-want-integration nil)
  (evil-want-keybinding nil)
  :init
  (evil-mode))

(use-package evil-collection :after evil :ensure t
  :config
  (evil-collection-init))

(use-package evil-magit
  :after (evil magit)
  :ensure t)

(use-package async :ensure t
  :config
  (setq async-bytecomp-allowed-packages '(all))
  (async-bytecomp-package-mode 1))

;; For GPG passphrase stuff
(defvar epa-pinentry-mode)
(if (string= system-type "windows-nt")
    (setf epa-pinentry-mode 'nil)
  (setf epa-pinentry-mode 'loopback))

(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file)
(setq backup-directory-alist '(("." . "~/.emacs.d/backups/")))
(setq inhibit-startup-screen t)
(setq-default indent-tabs-mode nil)
(fset 'yes-or-no-p 'y-or-n-p)
(global-set-key (kbd "<s-up>") 'toggle-frame-fullscreen)
(show-paren-mode)
(menu-bar-mode -1)

;;; Appearance

;; Theme
(use-package dracula-theme :ensure t :init (load-theme 'dracula t))

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

;; HTML

(use-package web-mode :ensure t :defer t
  :custom
  (web-mode-code-indent-offset 2)
  (web-mode-markup-indent-offset 2)
  (css-indent-offset 2)
  :mode "\\.html?\\'")

(use-package emmet-mode
  :ensure t
  :defer t
  :hook
  (web-mode . emmet-mode)
  (sgml-mode . emmet-mode)
  (css-mode . emmet-mode))

;; PHP

(use-package php-mode
  :ensure t
  :after (flycheck company)
  :init
  (defun my/php-mode-hook ()
    "Gets run on php-mode load."
    (php-eldoc-enable)
    (mapc #'flycheck-select-checker '(phpstan php php-phpcs))

    (set (make-local-variable 'company-backends)
          '(company-phpactor
            php-extras-company
            company-dabbrev-code
            company-files))

    (when (eq 0 (buffer-size))
      (insert "<?php\n\n")))
  :hook
  (php-mode . my/php-mode-hook)
  :custom
  (php-mode-coding-style 'psr2)
  (c-basic-offset 4))

(use-package php-extras :defer t :ensure t :after php-mode)

(use-package php-auto-yasnippets
  :defer t
  :ensure t
  :after php-mode
  :bind
  (:map php-mode-map
        ("C-c C-y" . yas/create-php-snippet)))

(use-package php-eldoc :ensure t :after php-mode)

(use-package flycheck-phpstan :ensure t :after (php-mode flycheck))

(use-package psysh :ensure t :after php-mode
  :config
  (evil-define-key '(normal insert) php-mode-map
    (kbd "C-c C-z") 'psysh))

(use-package php-refactor-mode :load-path "site-lisp/" :defer t
  :commands php-refactor-mode :init (add-hook 'php-mode-hook #'php-refactor-mode))
(use-package company-phpactor :load-path "site-lisp/phpactor.el")
(use-package phpactor :load-path "site-lisp/phpactor.el"
  :after (evil php-mode)
  :config
  (evil-define-key 'normal php-mode-map
    "gd" #'phpactor-goto-definition
    (kbd "<M-tab>") #'company-phpactor)
  (evil-define-key 'insert php-mode-map
    (kbd "<M-tab>") #'company-phpactor))
(use-package flycheck-phanclient :disabled :load-path "site-lisp/flycheck-phanclient")

(use-package phpunit :ensure t)
(use-package phpcbf :ensure t :config (setq phpcbf-standard "PSR2"))
(use-package phan :defer t)
(use-package fluca-php :load-path "site-lisp/")

(defun geben-php-run ()
  "Start the geben listener, then run the current script.
PHP is run with xdebug INI entries to point to geben listener."
  (interactive)
  (call-interactively #'geben)
  (let ((cmd (list "php"
               "-d" "xdebug.remote_enable=on"
               "-d" "xdebug.remote_host=127.0.0.1"
               "-d" "xdebug.remote_port=9000"
               "-d" "xdebug.remote_handler=dbgp"
               "-d" "xdebug.idekey=geben"
               "-d" "xdebug.remote_autostart=on"
               (buffer-file-name))))
    (apply #'start-process  "GebenPHPDebug" "*geben*" cmd)))

(use-package geben
  :ensure t
  :after evil
  :hook
  (geben-mode . evil-emacs-state)
  :config
  (evil-define-key 'normal php-mode-map
    (kbd "C-c C-d") #'geben-php-run))


;; JavaScript

(use-package js2-mode
  :ensure t
  :init
  (defun my/use-eslint-from-node-modules ()
    (let* ((root (locate-dominating-file
                  (or (buffer-file-name) default-directory)
                  "node_modules"))
           (eslint (and root
                        (expand-file-name "node_modules/eslint/bin/eslint.js"
                                          root))))
      (when (and eslint (file-executable-p eslint))
        (setq-local flycheck-javascript-eslint-executable eslint)
        (flycheck-select-checker 'javascript-eslint))))
  :hook
  (js2-mode . my/use-eslint-from-node-modules)
  :mode "\\.js\\'"
  :interpreter ("node" . js2-mode)
  :custom (js2-basic-offset 2))

(use-package tide
  :ensure t
  :after (flycheck)
  :custom
  (typescript-indent-level 2)
  (company-tooltip-align-annotations t)
  (flycheck-check-syntax-automatically '(save mode-enabled))
  (tide-format-options
   (list :indentSize 2
         :insertSpaceAfterFunctionKeywordForAnonymousFunctions t))
  :bind (:map tide-mode-map
              ("C-c C-r" . tide-rename-symbol)
              ("C-c r" . tide-refactor)
              ("C-M-i" . company-complete))
  :init
  (defun my/tide-hook ()
    (tide-setup)
    (tide-hl-identifier-mode)
    (set (make-local-variable 'company-backends)
         '(company-tide company-files)))
  :hook
  (js2-mode . my/tide-hook)
  (typescript-mode . my/tide-hook))

;; Python

(use-package anaconda-mode
  :defer t
  :ensure t
  :init
  (defun my/python-mode-hook ()
    "My python mode hook."
    (anaconda-mode)
    (anaconda-eldoc-mode)
    (add-to-list (make-local-variable 'company-backends) 'company-anaconda))
  (use-package company-anaconda :defer t :ensure t :after (company anaconda))
  :hook
  (python-mode . my/python-mode-hook)
  :custom
  (python-shell-interpreter
   (if (string-equal "windows-nt" system-type)
       "python"
     "python3")))

(use-package blacken
  :ensure t
  :after python
  :hook (python-mode . blacken-mode))

(use-package pipenv
  :defer t
  :ensure t
  :hook (python-mode . pipenv-mode))

(use-package blacken
  :defer t
  :ensure t
  :hook (python-mode . blacken-mode))

;; Other Modes

(use-package json-mode :ensure t :defer t :custom (js-indent-level 2))
(use-package csv-mode :ensure t :defer t)
(use-package markdown-mode :ensure t :defer t)
(use-package yaml-mode :ensure t :defer t)

;; Org-Mode
(use-package dot-org)

;; Useful Tools
(use-package magit :ensure t :defer t :bind (("C-x g" . magit-status)))
(use-package magithub :ensure t :after magit :config (magithub-feature-autoinject t))
(use-package undo-tree :ensure t :init (global-undo-tree-mode t))

(use-package company
  :ensure t
  :custom
  (company-dabbrev-downcase nil)
  (company-dabbrev-ignore-case nil)
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

(use-package ivy-hydra :defer t)

(use-package docker :ensure t)
(use-package flycheck :ensure t :defer t :init (global-flycheck-mode)
  :config (setq flycheck-phpcs-standard "PSR2"))
(use-package yasnippet :ensure t :init (use-package yasnippet-snippets :ensure t))
(use-package eldoc :config (global-eldoc-mode))
(use-package ace-window
  :ensure t
  :defer t
  :commands (ace-window)
  :bind
  ("M-i" . 'ace-window))

(provide 'init)
;;; init.el ends here
