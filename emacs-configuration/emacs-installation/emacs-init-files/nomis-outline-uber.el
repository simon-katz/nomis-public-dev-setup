;;; nomis-outline-uber -- -*- lexical-binding: t -*-

;;; nomis/outline-cycle-or-indent-or-complete

(defun nomis/outline-cycle-or-indent-or-complete (arg)
  (interactive "P")
  (if (and (bolp)
           (looking-at-p outline-regexp))
      (bicycle-cycle arg)
    ;; Maybe we could find what Tab would be bound to if `outline-minor-mode`
    ;; were not enabled. I've tried but it's non-trivial. So I'm not bothering,
    ;; at least for now.
    (company-indent-or-complete-common arg)))

;;; End

(provide 'nomis-outline-uber)
