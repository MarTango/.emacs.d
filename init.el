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
(when (version< emacs-version "27")
  (package-initialize))
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
(require 'bind-key)

;; Keybindings
(dolist (folder '("lisp" "site-lisp"))
        (add-to-list 'load-path (concat user-emacs-directory folder)))

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
  (evil-collection-init))

(use-package evil-magit
  :after (evil magit)
  :ensure t)

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

(use-package web-mode :ensure t :defer t
  :custom
  (web-mode-code-indent-offset 2)
  (web-mode-markup-indent-offset 2)
  (css-indent-offset 2)
  :mode "\\.html?\\'"
  :mode "\\.tsx\\'")

(use-package emmet-mode
  :ensure t
  :defer t
  :hook
  (web-mode . emmet-mode)
  (sgml-mode . emmet-mode)
  (css-mode . emmet-mode))


;; JavaScript
(use-package add-node-modules-path
  :ensure t
  :hook
  js2-mode
  tide-mode)

(use-package js2-mode
  :ensure t
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
   (list :indentSize 2))
  :bind (:map tide-mode-map
              ("C-c C-r" . tide-rename-symbol)
              ("C-c r" . tide-refactor)
              ("C-M-i" . company-complete)
              ("M-r" . tide-references))
  :init
  (defun my/tide-hook ()
    (tide-setup)
    (tide-hl-identifier-mode)
    (set (make-local-variable 'company-backends)
         '(company-tide company-files)))
  :config
  (flycheck-add-mode 'typescript-tslint 'web-mode)
  (flycheck-add-mode 'javascript-eslint 'web-mode)
  (flycheck-add-next-checker 'javascript-eslint 'javascript-tide 'append)
  :hook
  (js2-mode . my/tide-hook)
  (typescript-mode . my/tide-hook)
  (web-mode . (lambda () (when (string-equal "tsx" (file-name-extension buffer-file-name)) (my/tide-hook)))))

;; Python

(use-package anaconda-mode
  :defer t
  :ensure t
  :hook
  (python-mode . anaconda-mode)
  (python-mode . anaconda-eldoc-mode)
  (python-mode . (lambda ()
                   (add-to-list 'company-backends 'company-anaconda)
                   (message (buffer-name))
                   (when (not (string= " *temp*" (buffer-name)))
                     (flycheck-select-checker 'python-mypy))))
  :init
  (use-package company-anaconda :defer t :ensure t :after (company anaconda)))

(add-hook 'before-save-hook #'delete-trailing-whitespace)

(use-package pipenv
  :defer t
  :ensure t
  :config
  (defun pipenv-deactivate-flycheck ()
    "Deactivate integration of Pipenv with Flycheck."
    (setq flycheck-executable-find #'flycheck-default-executable-find))
  (defun pipenv-executable-find (executable)
    (if (bound-and-true-p python-shell-virtualenv-root)
        (locate-file executable  (python-shell-calculate-exec-path))
      (executable-find executable)))
  :hook
  (python-mode . pipenv-mode))

(use-package blacken
  :defer t
  :ensure t
  :hook
  (python-mode . blacken-mode))

;; Other Modes

(use-package json-mode :ensure t :defer t :custom (js-indent-level 2))
(use-package csv-mode :ensure t :defer t)
(use-package markdown-mode :ensure t :defer t)
(use-package yaml-mode :ensure t :defer t)

;; Org-Mode
(use-package dot-org)

;; Useful Tools
(use-package magit :ensure t :defer t :bind (("C-x g" . magit-status)))
(use-package forge
  :ensure t
  ;; :config
  ;; (setq gnutls-log-level 1) ;; See https://github.com/magit/ghub/issues/81
  )
;; (use-package magithub :disabled :ensure t :after magit :config (magithub-feature-autoinject t))
(use-package undo-tree :ensure t :init (global-undo-tree-mode t))

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
  :init
  :bind (:map projectile-mode-map
              ("C-c p" . projectile-command-map)))

(use-package counsel-projectile
  :after projectile
  :ensure t
  :init (counsel-projectile-mode))

(use-package ivy-hydra :defer t)

(use-package docker :ensure t)
(use-package flycheck :ensure t :defer t :init (global-flycheck-mode))
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
