;; Necessary preamble to use use-package
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
(require 'diminish)
(require 'bind-key)


;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ;;

;; Custom file
(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file)

;; My and third party emacs-lisp scripts.
(add-to-list 'load-path (concat user-emacs-directory "lisp/"))
(add-to-list 'load-path (concat user-emacs-directory "site-lisp/"))

;; Org Init File. Everything is in here!
(org-babel-load-file (concat user-emacs-directory "myconfig.org"))

;; Email
(require 'init-mu4e)

;; Org/Google-Calendar integration.
(require 'init-orggcal)
