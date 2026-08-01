;;; Init stuff --- nomis-magit  -*- lexical-binding: t; -*-

;;; Code:

(require 'nomis-magit-fixes)
(require 'nomis-magit-non-lexical)
(require 'nomis-magit-difftastic)

;;; magit-auto-revert-mode

;; See https://emacs.stackexchange.com/questions/35701/magit-sets-auto-revert-mode-annoying
;; - Don't globally set auto-revert-mode (that's very rude!).

;; (magit-auto-revert-mode 0)

;;; Key bindings

(with-eval-after-load 'magit-diff
  ;; Add H key to some existing key bindings to get "other-window".
  (define-key magit-hunk-section-map (kbd "<kp-enter>") ; H-<return>
    'magit-diff-visit-file-other-window)
  (define-key magit-hunk-section-map (kbd "<C-kp-enter>") ; H-C-<return>
    'magit-diff-visit-worktree-file-other-window))

;;; magit-list-refs-sortby

(setq magit-list-refs-sortby "-creatordate")

;;; Don't truncate lines

(defun -nomis/magit/no-truncate-lines ()
  (toggle-truncate-lines 1))

(add-hook 'git-rebase-mode-hook '-nomis/magit/no-truncate-lines)

;;; End

(provide 'nomis-magit)
