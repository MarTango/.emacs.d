;;; dot-org.el --- My org-mode configuration
;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:

;;; Commentary:
;; My org-mode related configurations.

;;; Code:
(use-package org :ensure t
  :bind
  (("C-c l" . org-store-link)
   ("C-c a" . org-agenda)
   ("C-c c" . org-capture)
   ("C-c b" . org-iswitchb)
   ("C-c C-x" . org-clock-goto)
   ;; ("C-'" . org-cycle-agenda-files)
   )
  :custom
  (org-src-preserve-indentation t)
  (org-directory "~/org")
  (org-capture-templates '(("a" "Add to inbox" entry (file "~/org/inbox.org")
                            "* TODO %?\n  :PROPERTIES:\n  :CREATED: %U\n  :END:")
                           ("l" "Add to inbox with link" entry (file "~/org/inbox.org")
                            "* TODO %?\n  :PROPERTIES:\n  :CREATED: %U\n  :END:\n  %a")
                           ("n" "Add a note" entry (file "~/org/notes.org")
                            "* %?\n  %a")
                           ("p" "Protocol" entry (file "~/org/inbox.org")
                            "* %^{Title}\nSource: %u, %c\n #+BEGIN_QUOTE\n%i\n#+END_QUOTE\n\n\n%?")
                           ("L" "Protocol Link" entry (file "~/org/inbox.org")
                            "* %? [[%:link][%:description]] \nCaptured On: %U")))
  (org-refile-targets '(("~/org/todo.org" :maxlevel . 3)))
  (org-refile-use-outline-path t)
  (org-outline-path-complete-in-steps nil)
  (org-agenda-clockreport-parameter-plist '(:link t :maxlevel 7))
  (org-agenda-files '("~/org/inbox.org" "~/org/todo.org")))

;; Some require statements to suppress some flycheck warnings - I
;; mean to prevent byte-compilation errors.

;; (require 'advice)
;;(require 'evil-org)
;; (require 'evil-org-agenda)

(use-package evil-org
  :ensure t
  :after (evil org)
  :hook
  (org-mode . evil-org-mode)
  (evil-org-mode . evil-org-set-key-theme)
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(use-package org-protocol :disabled)
(use-package org-tempo)
(use-package ob-rust)
(use-package ob-graphql)

;; active Babel languages
(org-babel-do-load-languages
 'org-babel-load-languages
 '((sql . t)
   (python .t)
   (emacs-lisp . t)
   (rust . t)
   (shell . t)))
;; add additional languages with '((language . t)))

(provide 'dot-org)
;;; dot-org.el ends here
