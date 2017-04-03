(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("marmalade" . "http://marmalade-repo.org/packages/")
			 ("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)

(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file)

;;; Mode-specific init files:
(add-to-list 'load-path (concat user-emacs-directory "/lisp"))

;; Zenburn
(use-package zenburn-theme
  :ensure t)

;; Company
(use-package company
  :defer t
  :ensure t
  :config
  (global-company-mode)
  (setq company-dabbrev-downcase nil)
  )

;; Flycheck
(use-package flycheck
  :defer t
  :ensure t
  :config
  (global-flycheck-mode)
  )


;; Magit
(use-package magit
  :bind (("C-x g" . magit-status)))


;; General
(global-set-key (kbd "<s-up>") 'toggle-frame-fullscreen)
(global-eldoc-mode)
(show-paren-mode)
(setq inhibit-startup-screen t)
(global-column-enforce-mode t)

;; JavaScript
(require 'init-js2)

;; Magit
(global-set-key (kbd "C-x g") 'magit-status)
(require 'magithub)

;; mu4e
(require 'init-mu4e)

;; Org
(require 'init-orgmode)

;; Org-gcal
(require 'init-orggcal)

;; PHP
(require 'init-php)

;; Python
(setq elpy-rpc-python-command "python3")
(elpy-enable)

;; Theme
(load-theme 'zenburn)

;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
;;; init ends here
