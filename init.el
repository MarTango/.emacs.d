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
  :ensure t
  :bind (("C-x g" . magit-status)))
(use-package magithub)

(use-package eldoc
  :config
  (global-eldoc-mode))

;; General
(global-set-key (kbd "<s-up>") 'toggle-frame-fullscreen)
;; (global-eldoc-mode)
(show-paren-mode)
(setq inhibit-startup-screen t)
(global-column-enforce-mode t)

(require 'init-js2)
(require 'init-mu4e)
(require 'init-orgmode)
(require 'init-orggcal)
(require 'init-php)

;; Python
(use-package elpy
  :defer t
  :ensure t
  :interpreter "python3"
  :config
  (elpy-enable))

;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
;;; init ends here
