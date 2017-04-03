(fset 'yes-or-no-p 'y-or-n-p)
(global-set-key (kbd "<s-up>") 'toggle-frame-fullscreen)
(show-paren-mode)
(setq backup-directory-alist '(("." . "~/.emacs.d/backups/")))
;; Zenburn
(use-package zenburn-theme
  :ensure t)

(use-package org
  :ensure org-plus-contrib
  :bind (("C-c l" . org-store-link)
	 ("C-c a" . org-agenda)
	 ("C-c c" . org-capture)
	 ("C-c b" . org-iswitchb))
  :init
  (setq org-directory "~/orgfiles"
	org-default-notes-file (concat org-directory "/notes.org")
	org-capture-templates '(("t" "Task" entry (file "~/orgfiles/tasks.org")
				 "* TODO %?\n  %i\n  %a")
				("e" "Calendar Event" entry (file "~/orgfiles/gcal.org")
				 "* %?\n\n%^T\n\n")
				("o" "Trello Card" entry (file "~/orgfiles/trello.org")
				 "* To-Do %?\n %i\n %a")))
  )

(use-package org-trello
  :ensure t
  :defer t
  :config
  (setq org-trello-current-prefix-keybinding "C-c o"))

;; Company
(use-package company
  :ensure t
  :config
  (global-company-mode)
  (setq company-dabbrev-downcase nil)
  )
;; Column Enforce
(use-package column-enforce-mode :defer t :ensure t)

;; Flycheck
(use-package flycheck
  :defer t
  :ensure t
  :config
  (global-flycheck-mode)
  )

(use-package eldoc
  :config
  (global-eldoc-mode))

(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)))
(use-package magithub
  :after magit
  :config (magithub-feature-autoinject t))

(use-package php-auto-yasnippets
  :defer t
  :ensure t
  :commands php-mode
  :bind (:map php-mode-map
	      ("C-c C-y" . yas/create-php-snippet))
  )

(use-package php-refactor-mode :defer t)

(use-package phpcbf :ensure t :defer t
  :config
  (setq phpcbf-standard 'PSR2))

(use-package phpunit :ensure t :defer t)

(use-package phan :defer t)

(use-package ggtags :ensure t)

(use-package php-boris :ensure t)

(use-package php-mode :defer t :ensure t :mode "\\.php$"
  :init
  (add-hook 'php-mode-hook
	    (lambda ()
	      (php-refactor-mode)
	      (ggtags-mode)
	      (php-eldoc-enable)
	      (php-enable-psr2-coding-style)
	      (column-enforce-mode)
	      (add-to-list 'company-backends '(company-gtags php-extras-company))
	      )
	    )
  )

(use-package fluca-php
  :load-path "site-lisp/")

(use-package company-tern
  :ensure t
  )

(use-package js2-mode
  :ensure t
  :defer t
  :mode "\\.js\\'"
  :interpreter "node"
  :init
  (add-hook 'js2-mode-hook (lambda ()
			     (tern-mode)
			     (column-enforce-mode)))
  :config 
  (add-to-list 'company-backends 'company-tern))

(use-package elpy
  :defer t
  :ensure t
  :interpreter "python3"
  :config
  (elpy-enable))

(use-package json-mode :ensure t)
(use-package csv-mode :ensure t)
(use-package markdown-mode :ensure t)
