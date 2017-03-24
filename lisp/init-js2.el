;;; Code:
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'load-path "/usr/local/lib/node_modules/tern/emacs/")
(autoload 'tern-mode "tern.el" nil t)
(add-hook 'js2-mode-hook (lambda () (tern-mode t)))
(add-to-list 'company-backends 'company-tern)
(provide 'init-js2)
;;; Commentary:

;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
;;; init-js2 ends here
