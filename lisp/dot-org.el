;;; dot-org.el --- My org-mode configuration
;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; Commentary:
;; My org-mode related configurations.

;;; Code:
(use-package org :ensure org-plus-contrib
  :bind
  (("C-c l" . org-store-link)
   ("C-c a" . org-agenda)
   ("C-c c" . org-capture)
   ("C-c b" . org-iswitchb)
   ("C-c C-x" . org-clock-goto)
   ("C-'" . org-cycle-agenda-files))
  :custom
  (org-directory "~/gtd")
  (org-capture-templates '(("a" "Add to inbox" entry (file "~/gtd/inbox.org")
                            "* TODO %?\n  :PROPERTIES:\n  :CREATED: %U\n  :END:")
                           ("l" "Add to inbox with link" entry (file "~/gtd/inbox.org")
                            "* TODO %?\n  :PROPERTIES:\n  :CREATED: %U\n  :END:\n  %a")
                           ("n" "Add a note" entry (file "~/gtd/notes.org")
                            "* %?\n  %a")
                           ("p" "Protocol" entry (file "~/gtd/inbox.org")
                            "* %^{Title}\nSource: %u, %c\n #+BEGIN_QUOTE\n%i\n#+END_QUOTE\n\n\n%?")
                           ("L" "Protocol Link" entry (file "~/gtd/inbox.org")
                            "* %? [[%:link][%:description]] \nCaptured On: %U")))
  (org-refile-targets '(("~/gtd/todo.org" :maxlevel . 3)
                        ("~/gtd/tickler.org" :maxlevel . 2)))
  (org-refile-use-outline-path t)
  (org-outline-path-complete-in-steps nil)
  (org-agenda-clockreport-parameter-plist '(:link t :maxlevel 4))
  (org-agenda-files '("~/gtd/inbox.org" "~/gtd/todo.org" "~/gtd/tickler.org" "~/gtd/notes.org")))

;; Some require statements to suppress some flycheck warnings - I
;; mean to prevent byte-compilation errors.

(require 'advice)
(require 'evil-org)
(require 'evil-org-agenda)

(use-package evil-org
  :ensure t
  :after (evil org)
  :hook
  (org-mode . evil-org-mode)
  (evil-org-mode . evil-org-set-key-theme)
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(use-package server
  :no-require
  :hook (after-init . server-start))

(use-package org-protocol)

(provide 'dot-org)
;;; dot-org ends here
