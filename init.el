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
(require 'bind-key)

;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ;;
(use-package async :ensure t :init (setq async-bytecomp-allowed-packages '(all)) (async-bytecomp-package-mode 1))
;; Org Init File. Everything is in here!
(org-babel-load-file (concat user-emacs-directory "myconfig.org"))

;; For GPG passphrase stuff
(defvar epa-pinentry-mode)
(if (string= system-type "windows-nt")
    (setf epa-pinentry-mode 'nil)
  (setf epa-pinentry-mode 'loopback))

;; flycheck-phan
;; (use-package flycheck-phanclient :load-path "/Users/martintang/.emacs.d/lisp/flycheck-phanclient/")
;; (add-to-list 'load-path "/Users/martintang/.emacs.d/lisp/flycheck-phanclient/")
;; (require 'flycheck-phanclient)
