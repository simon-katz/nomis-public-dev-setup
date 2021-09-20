;;; nomis-diff-hl.el --- diff-hl setup -*- lexical-binding: t; -*-

;;;; ___________________________________________________________________________
;;;; ---- diff-hl-mode ----

(require 'diff-hl)

(add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
(add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)

(global-diff-hl-mode)
(diff-hl-flydiff-mode)
(unless (window-system) (diff-hl-margin-mode))
(global-diff-hl-show-hunk-mouse-mode)

(add-hook 'dired-mode-hook 'diff-hl-dired-mode-unless-remote)

(setq diff-hl-show-hunk-function
      (case :other
        (:default 'diff-hl-show-hunk-inline-popup)
        (:other   'diff-hl-show-hunk-posframe)))

;;;; ___________________________________________________________________________

(provide 'nomis-diff-hl)
