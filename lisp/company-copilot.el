;;; company-copilot.el --- company + copilot -*- lexical-binding: t -*-

;; Author: Martin Tang

;; Package-requires: ((emacs "27.2") (company "0.9.0") (copilot "0.1.0"))


(defun company-copilot (command &optional arg &rest ignored)
  "copilot.el backend for `company-mode'.
See `company-backends' for more info about COMMAND and ARG."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-copilot))
    (prefix (and (eq major-mode 'python-mode)
                 (company-grab-symbol)))
    (candidates (company-copilot--candidates arg))
    (meta (company-copilot--meta arg))
    (annotation (company-copilot--annotation arg))
    (doc-buffer (company-copilot--doc-buffer arg))
    (location (company-copilot--location arg))
    (post-completion (company-copilot--post-completion arg))))


(defun company-copilot--candidates (prefix)
  "Return a list of candidates for PREFIX."
  (let ((candidates (copilot--candidates prefix)))
    (if (and (listp candidates)
             (stringp (car candidates)))
        candidates
      (list (format "%s" candidates)))))

(defun company-copilot--annotation (candidate)
  "Return the annotation for CANDIDATE."
  (let ((annotation (copilot--annotation candidate)))
    (if (stringp annotation)
        annotation
      (format "%s" annotation))))

(defun company-copilot--post-completion (candidate)
  "Perform post-completion actions for CANDIDATE."
  (let ((post-completion (copilot--post-completion candidate)))
    (when (stringp post-completion)
      (insert post-completion))))

(defun company-copilot--doc-buffer (candidate)
  "Return the doc-buffer for CANDIDATE."
  (let ((doc-buffer (copilot--doc-buffer candidate)))
    (when (stringp doc-buffer)
      (with-current-buffer (get-buffer-create "*copilot-doc*")
        (erase-buffer)
        (insert doc-buffer)
        (current-buffer)))))


(provide 'company-copilot)
