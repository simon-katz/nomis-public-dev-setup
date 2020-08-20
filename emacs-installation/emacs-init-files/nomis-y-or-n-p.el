;;;; nomis-y-or-n-p.el --- nomis y-or-n-p tailoring ---  -*- lexical-binding: t -*-

;;;; ___________________________________________________________________________

(defalias 'yes-or-no-p 'y-or-n-p)

;;;; ___________________________________________________________________________

(defun nomis/y-or-n-p-with-quit->nil (prompt)
  (condition-case nil
      (y-or-n-p prompt)
    (quit nil)))

;;;; ___________________________________________________________________________

(defvar nomis/y-or-n-p-query-replace-map-with-ret-for-y
  (let* ((m (copy-keymap query-replace-map)))
    (define-key m (kbd "RET") 'act)
    m))

(advice-add
 'y-or-n-p
 :around
 (lambda (orig-func &rest args)
   (let* ((query-replace-map nomis/y-or-n-p-query-replace-map-with-ret-for-y))
     (apply orig-func args)))
 '((name . nomis/ret-for-y)))

;;;; ___________________________________________________________________________

(provide 'nomis-y-or-n-p)
