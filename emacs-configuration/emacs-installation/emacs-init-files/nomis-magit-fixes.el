;;;; Init stuff -- Fix problems with Magit.

;;;; ___________________________________________________________________________
;;;; ---- nomis/revert-all-unmodified-buffers-in-git-repo ----

(defun nomis/vc/buffer-in-current-repo? (b)
  (s-starts-with? (nomis/dirtree/vc-root-dir)
                  (buffer-file-name b)))

(defun nomis/revert-all-unmodified-buffers-in-git-repo (inhibit-message?)
  "Refreshes all open unmodified buffers in current buffer's Git repo
 from their files."
  (interactive "P")
  (let* ((this-buffer (current-buffer)))
    (cl-flet ((repository-buffer?
               (b)
               (with-current-buffer this-buffer
                 (case 2
                   (1 (magit-auto-revert-repository-buffer-p b))
                   (2 (nomis/vc/buffer-in-current-repo? b))))))
      (-nomis/revert-all-buffers (lambda (b)
                                   (and (not (buffer-modified-p b))
                                        (repository-buffer? b)))
                                 inhibit-message?))))

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
