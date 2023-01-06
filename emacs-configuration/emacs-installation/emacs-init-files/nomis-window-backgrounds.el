;;;; Init stuff --- Window backgrounds ---  -*- lexical-binding: t -*-

;;;; ___________________________________________________________________________
;;;; ---- Use a different background for unselected windows ----

(defun -nomis/window-backgrounds/turn-on-auto-dim ()
  (when (fboundp 'auto-dim-other-buffers-mode)
    (auto-dim-other-buffers-mode t)))

(add-hook 'after-init-hook
          '-nomis/window-backgrounds/turn-on-auto-dim)

;;;; ___________________________________________________________________________

(provide 'nomis-window-backgrounds)
