;;; dot-org.el --- My org-mode configuration
;;; Commentary:

;;; Code:
(use-package org :ensure org-plus-contrib
  :defer t
  :bind
  (("C-c l" . org-store-link)
   ("C-c a" . org-agenda)
   ("C-c c" . org-capture)
   ("C-c b" . org-iswitchb)
   ("C-c C-x" . org-clock-goto)
   ("C-'" . org-cycle-agenda-files))
  :init
  (setq org-directory "~/gtd"
        org-capture-templates '(("a" "Add to inbox" entry (file "~/gtd/inbox.org")
                                 "* TODO %?
:PROPERTIES:
:CREATED: %U
:END:")
                                ("p" "Protocol" entry (file "~/gtd/inbox.org")
                                 "* %^{Title}\nSource: %u, %c\n #+BEGIN_QUOTE\n%i\n#+END_QUOTE\n\n\n%?")
                                ("L" "Protocol Link" entry (file "~/gtd/inbox.org")
                                 "* %? [[%:link][%:description]] \nCaptured On: %U"))
        org-refile-targets '(("~/gtd/todo.org" :maxlevel . 3)
                             ("~/gtd/tickler.org" :maxlevel . 2))
        org-agenda-files '("~/gtd/inbox.org" "~/gtd/todo.org" "~/gtd/tickler.org")))

(use-package evil-org :ensure t :after (evil org)
  :config
  (add-hook 'org-mode-hook #'evil-org-mode)
  (add-hook 'evil-org-mode-hook #'evil-org-set-key-theme)
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(use-package server
  :no-require
  :hook (after-init . server-start))

(use-package org-protocol)

(provide 'dot-org)
;;; dot-org ends here
