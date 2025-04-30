;;;; Init stuff --- Window backgrounds ---  -*- lexical-binding: t -*-

;;;; ___________________________________________________________________________
;;;; ---- Use a different background for unselected windows ----

(defun -nomis/window-backgrounds/turn-on-auto-dim ()
  (when (fboundp 'auto-dim-other-buffers-mode)
    (auto-dim-other-buffers-mode t)))

;; (add-hook 'after-init-hook
;;           '-nomis/window-backgrounds/turn-on-auto-dim)

;;;; ___________________________________________________________________________

(defun nomis/auto-dim/with-turn-off-and-restore* (f)
  (let* ((auto-dimming? (bound-and-true-p auto-dim-other-buffers-mode)))
    (when auto-dimming? (auto-dim-other-buffers-mode 0))
    (unwind-protect
        (funcall f)
      (when auto-dimming? (auto-dim-other-buffers-mode 1)))))

(cl-defmacro nomis/auto-dim/with-turn-off-and-restore (&body body)
  (declare (indent 0))
  `(nomis/auto-dim/with-turn-off-and-restore* (lambda () ,@body)))

;;;; ___________________________________________________________________________

(provide 'nomis-window-backgrounds)
