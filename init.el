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

;; Org Init File. Everything is in here!
(org-babel-load-file (concat user-emacs-directory "myconfig.org"))

;; Email
(use-package init-mu4e :load-path "lisp/" :commands mu4e)

;; Org/Google-Calendar integration.
(use-package init-orggcal :load-path "lisp/")

(use-package ghub-credentials :load-path "lisp/")
