;;;; nomis-y-or-n-p.el --- nomis y-or-n-p tailoring ---  -*- lexical-binding: t -*-

;;;; ___________________________________________________________________________

(defalias 'yes-or-no-p 'y-or-n-p)

;;;; ___________________________________________________________________________

(defun nomis/y-or-n-p-with-quit->nil (prompt)
  (condition-case nil
      (y-or-n-p prompt)
    (quit nil)))

;;;; ___________________________________________________________________________

(provide 'nomis-y-or-n-p)
