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
(setq package-enable-at-startup nil)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
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
(add-to-list 'load-path (concat user-emacs-directory "lisp"))
(add-to-list 'load-path (concat user-emacs-directory "site-lisp"))

(use-package evil :ensure t :init (setq evil-want-integration nil) (evil-mode))
(use-package evil-collection :after evil :ensure t
  :config
  (evil-collection-init))
(use-package evil-magit :after (evil magit) :ensure t)

(use-package avy
  :bind
  (("C-:" . avy-goto-char)))


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

(defun my/frame-behaviours (&optional frame)
  "Set appearance of FRAME depending on terminal or GUI."
  (with-selected-frame (or frame (selected-frame))
    (unless window-system
      (set-face-background 'default "nil" frame))))

(add-hook 'after-make-frame-functions #'my/frame-behaviours)

;;; Packages

;; HTML

(use-package web-mode :ensure t :defer t
  :config
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-markup-indent-offset 2)
  (defvar css-indent-offset 2)
  :mode "\\.html?\\'")

(use-package emmet-mode :ensure t :defer t :init
  (add-hook 'web-mode-hook #'emmet-mode)
  (add-hook 'sgml-mode-hook #'emmet-mode)
  (add-hook 'css-mode-hook #'emmet-mode))

;; PHP

(defun my/php-mode-hook ()
  "Gets run on php-mode load."
  (php-eldoc-enable)
  (make-local-variable 'company-backends)
  (add-to-list 'company-backends '(company-phpactor php-extras-company company-dabbrev-code))
  (flycheck-select-checker 'phpstan)
  (setq php-mode-coding-style 'psr2
        c-basic-offset 4)
  (when (eq 0 (buffer-size))
    (insert "<?php\n\n")))

(use-package php-mode :ensure t
  :init (add-hook 'php-mode-hook #'my/php-mode-hook))
(use-package php-extras :defer t :ensure t :after php-mode)
(use-package php-auto-yasnippets :defer t :ensure t :after php-mode
  :bind (:map php-mode-map ("C-c C-y" . yas/create-php-snippet)))
(use-package php-eldoc :ensure t :after php-mode)
(use-package flycheck-phpstan :ensure t :after (php-mode flycheck))
;; (use-package ggtags :defer t :ensure t :init (add-hook 'php-mode-hook #'ggtags-mode))
(use-package psysh :ensure t :after php-mode
  :config
  (evil-define-key '(normal insert) php-mode-map
    (kbd "C-c C-z") 'psysh)
  )

(use-package php-refactor-mode :load-path "site-lisp/" :defer t
  :commands php-refactor-mode :init (add-hook 'php-mode-hook #'php-refactor-mode))
(use-package company-phpactor :load-path "site-lisp/phpactor.el")
(use-package phpactor :load-path "site-lisp/phpactor.el"
  :after (evil php-mode)
  :config
  (evil-define-key 'normal php-mode-map
    "gd" #'phpactor-goto-definition
    (kbd "<S-tab>") #'company-phpactor))
(use-package flycheck-phanclient :disabled :load-path "site-lisp/flycheck-phanclient")

(use-package phpunit :ensure t)
(use-package phpcbf :ensure t :config (setq phpcbf-standard "PSR2"))
(use-package phan :defer t)
(use-package fluca-php :load-path "site-lisp/")

(defun geben-php-run ()
  "Start the geben listener, then run the current script with xdebug configuration to point to geben listener."
  (interactive)
  (call-interactively #'geben)
  (let ((cmd (list "php" "-d"
                   "xdebug.remote_enable=on" "-d"
                   "xdebug.remote_host=127.0.0.1" "-d"
                   "xdebug.remote_port=9000" "-d"
                   "xdebug.remote_handler=dbgp" "-d"
                   "xdebug.idekey=geben" "-d"
                   "xdebug.remote_autostart=On"
                   (buffer-file-name))))
    (apply #'start-process  "GebenPHPDebug" "*geben*" cmd)))

(use-package geben
  :ensure t
  :after evil
  :init
  (add-hook 'geben-mode-hook #'evil-emacs-state)
  :config
  (evil-define-key 'normal php-mode-map
    (kbd "C-c C-d") #'geben-php-run))


;; JavaScript

(defun my/js2-mode-hook ()
  "My javascript mode hook."
  (setq js2-basic-offset 2)
  (tern-mode t)
  (js2-refactor-mode)
  (add-to-list (make-local-variable 'company-backends) 'company-tern))

(use-package js2-mode
  :ensure t
  :mode "\\.js\\'"
  :interpreter ("node" . js2-mode)
  :init
  (add-hook 'js2-mode-hook #'my/js2-mode-hook))

(use-package tern
  :ensure t
  :defer t
  :config
  (evil-define-key 'normal tern-mode-keymap
    "gd" #'tern-find-definition
    (kbd "C-t") #'tern-pop-find-definition))

(use-package company-tern :ensure t :after tern)
(use-package js2-refactor :ensure t :after js2-mode
  :config (js2r-add-keybindings-with-prefix "C-c C-m"))


;; Python

(defun my/python-mode-hook ()
  "My python mode hook."
  (anaconda-mode)
  (anaconda-eldoc-mode)
  (add-to-list (make-local-variable 'company-backends) 'company-anaconda))

(use-package anaconda-mode :defer t :ensure t :init (add-hook 'python-mode-hook #'my/python-mode-hook))
(use-package company-anaconda :defer t :ensure t :after (company anaconda))

;; Languages I don't use.

(use-package ess :disabled :defer t)
(use-package octave-mode :disabled :defer t :mode "\\.m\\'")

;; Other Modes

(use-package json-mode :ensure t :defer t :init (add-hook 'json-mode-hook (lambda () (setq js-indent-level 2))))
(use-package csv-mode :ensure t :defer t)
(use-package markdown-mode :ensure t :defer t)
(use-package yaml-mode :ensure t :defer t)

;; Org-Mode
(use-package dot-org)

;; Mu4e
(defvar my/mu4e-load-path "/usr/local/share/emacs/site-lisp/mu/mu4e"
  "Directory containing mu4e emacs-lisp files.")
(use-package mu4e
  :load-path my/mu4e-load-path
  :custom
  (mu4e-maildir "~/.email/work")
  (mu4e-sent-folder "/Sent")
  (mu4e-trash-folder "/Trash")
  (mu4e-drafts-folder "/drafts")
  (mu4e-get-mail-command "mbsync -a")
  (mu4e-change-filenames-when-moving t)
  (mu4e-update-interval 180)
  (mu4e-view-show-images t)
  :config
  (setenv "GPG_AGENT_INFO" "~/.gnupg/S.gpg-agent:2300:1"))

(use-package smtpmail
  :custom
  (user-mail-address "martin@brainlabsdigital.com")
  (user-full-name "Martin Tang")
  (message-send-mail-function 'smtpmail-send-it)
  (starttls-use-gnutls t)
  (smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil)))
  (smtpmail-auth-credentials '(("smtp.gmail.com" 587 "martin@brainlabsdigital.com" nil)))
  (smtpmail-default-smtp-server "smtp.gmail.com")
  (smtpmail-smtp-service 587)
  (message-kill-buffer-on-exit t))

(use-package mu4e-alert
  :ensure t
  :hook (after-init . mu4e-alert-enable-mode-line-display))

(use-package org-mu4e
  :load-path my/mu4e-load-path
  :custom (org-mu4e-convert-to-html t))

;; Useful Tools
(use-package magit :ensure t :defer t :bind (("C-x g" . magit-status)))
(use-package magithub :ensure t :after magit :config (magithub-feature-autoinject t))
(use-package undo-tree :ensure t :init (global-undo-tree-mode t))
(use-package company :ensure t :config
  (global-company-mode)
  (setq company-dabbrev-downcase nil
        company-dabbrev-ignore-case nil))
(use-package which-key :ensure t :init (which-key-mode 1))

(use-package counsel
  :ensure t
  :init
  (ivy-mode 1)
  :config
  (global-set-key (kbd "C-s") 'swiper)
  (global-set-key (kbd "M-x") 'counsel-M-x)
  (global-set-key (kbd "C-x C-f") 'counsel-find-file)
  (global-set-key (kbd "<f1> f") 'counsel-describe-function)
  (global-set-key (kbd "<f1> v") 'counsel-describe-variable)
  (global-set-key (kbd "<f1> l") 'counsel-find-library)
  (global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
  (setq magit-completing-read-function 'ivy-completing-read))

(use-package ivy-hydra :defer t)

(use-package docker :ensure t)
(use-package flycheck :ensure t :defer t :init (global-flycheck-mode)
  :config (setq flycheck-phpcs-standard "PSR2"))
(use-package yasnippet :ensure t :init (use-package yasnippet-snippets :ensure t))
(use-package eldoc :config (global-eldoc-mode))
(use-package ace-window :ensure t :defer t :commands (ace-window) :init (global-set-key (kbd "M-i") 'ace-window))

(provide 'init)
;;; init.el ends here
