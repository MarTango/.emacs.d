(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives
	     '("marmalade" . "http://marmalade-repo.org/packages/"))
(when (< emacs-major-version 24)
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
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
    (evil json-mode company-tern js2-mode org-gcal org-trello magithub magit ess php-auto-yasnippets flycheck php-extras php-eldoc ggtags zenburn-theme elpy php-refactor-mode phpcbf phpunit php-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;; Packages:
(add-to-list 'load-path (concat user-emacs-directory "/lisp"))

;; Company
(global-company-mode)

;; Flycheck
(global-flycheck-mode)

;; General
(global-set-key (kbd "<s-up>") 'toggle-frame-fullscreen)
(ivy-mode)
(global-eldoc-mode)

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
(elpy-enable)

;; Theme
(load-theme 'zenburn)

;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
;;; init ends here
