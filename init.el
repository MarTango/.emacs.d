(require 'package) ;; You might already have this line
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives
	     '("marmalade" . "http://marmalade-repo.org/packages/"))
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
  )
(package-initialize) ;; You might already have this line

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("f5512c02e0a6887e987a816918b7a684d558716262ac7ee2dd0437ab913eaec6" default)))
 '(org-agenda-files
   (quote
    ("~/orgfiles/gcal.org" "~/Expedia/summary.org" "~/orgfiles/trello.org" "~/orgfiles/tasks.org")))
 '(org-trello-current-prefix-keybinding "C-c o" nil (org-trello))
 '(package-selected-packages
   (quote
    (json-mode company-tern js2-mode org-gcal org-trello magithub magit ess php-auto-yasnippets flycheck php-extras php-eldoc ggtags zenburn-theme elpy php-refactor-mode phpcbf phpunit php-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; General
(global-set-key (kbd "<s-up>") 'toggle-frame-fullscreen)
(ivy-mode)
(add-to-list 'load-path (concat user-emacs-directory "/lisp"))
(global-eldoc-mode)

;; Magit
(global-set-key (kbd "C-x g") 'magit-status)
(require 'magithub)

;; Company
(global-company-mode t)

;; Flycheck
(global-flycheck-mode t)

;; Theme
(load-theme 'zenburn)

;; Org
(require 'init-orgmode)

;; Org-gcal
(require 'init-orggcal)

;; PHP
(require 'init-php)

;; Python
(elpy-enable)

;; JavaScript
(require 'init-js2)

;; mu4e
(require 'init-mu4e)

;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
;;; init ends here
