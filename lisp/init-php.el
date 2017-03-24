;;; package --- Summary
;; My init for PHP.
;;; Code:
(add-to-list 'auto-mode-alist '("\\.php$" . php-mode))
(require 'php-auto-yasnippets)
(define-key php-mode-map (kbd "C-c C-y") 'yas/create-php-snippet)
(add-hook 'php-mode-hook
	  (lambda ()
	    (php-refactor-mode)
	    (ggtags-mode)
	    (php-enable-psr2-coding-style)
	    (php-eldoc-enable)
	    (setq show-trailing-whitespace nil)))
(setq phpcbf-standard 'PSR2)
(provide 'init-php)

;;; Commentary:

;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
;;; init-php ends here
