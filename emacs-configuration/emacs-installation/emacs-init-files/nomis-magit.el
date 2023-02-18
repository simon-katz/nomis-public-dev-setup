;;;; Init stuff -- nomis-magit  -*- lexical-binding: t; -*-

;;;; ___________________________________________________________________________

(require 'nomis-magit-fixes)
(require 'nomis-magit-non-lexical)
(require 'nomis-magit-difftastic)

;;;; ___________________________________________________________________________

;;;; See https://emacs.stackexchange.com/questions/35701/magit-sets-auto-revert-mode-annoying
;;;;
;;;; Don't globally set auto-revert-mode (that's very rude!).

;; (magit-auto-revert-mode 0)

;;;; ___________________________________________________________________________

(with-eval-after-load 'magit-diff
  ;; Add H key to some existing key bindings to get "other-window".
  (define-key magit-hunk-section-map (kbd "<kp-enter>") ; H-<return>
    'magit-diff-visit-file-other-window)
  (define-key magit-hunk-section-map (kbd "<C-kp-enter>") ; H-C-<return>
    'magit-diff-visit-worktree-file-other-window))

;;;; ___________________________________________________________________________

(provide 'nomis-magit)
