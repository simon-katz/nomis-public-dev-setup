;;;; Init stuff -- Fix problems with Magit.

;;;; ___________________________________________________________________________
;;;; ---- nomis/revert-all-unmodified-buffers-in-git-repo ----

(defun nomis/revert-all-unmodified-buffers-in-git-repo (inhibit-message?)
  "Refreshes all open unmodified buffers in current buffer's Git repo
 from their files."
  (interactive "P")
  (-nomis/revert-all-buffers (lambda (b)
                               (and (not (buffer-modified-p b))
                                    (magit-auto-revert-repository-buffer-p b)))
                             inhibit-message?))

;;;; ___________________________________________________________________________
;;;; ---- -nomis/hack-magit-auto-revert ----

;;;; See https://emacs.stackexchange.com/questions/35701/magit-sets-auto-revert-mode-annoying
;;;;
;;;; Don't globally set auto-revert-mode (that's very rude!). Instead, revert
;;;; buffers in the repo after each Magit operation.

(magit-auto-revert-mode 0)

(with-eval-after-load 'magit-mode
  (cond
   ((member (magit-version) '("20210913.1931"))
    (advice-add
     'magit-auto-revert-buffers
     :around
     (lambda (_orig-fun &rest _args)
       (nomis/revert-all-unmodified-buffers-in-git-repo t))
     '((name . -nomis/hack-magit-auto-revert))))
   (t
    (message-box (s-join " "
                         '("Revisit `-nomis/hack-magit-auto-revert`"
                           "for this version of Magit."))))))


;;;; ___________________________________________________________________________

(provide 'nomis-magit-fixes)
