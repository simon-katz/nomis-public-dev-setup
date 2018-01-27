;;;; Init stuff -- Fix problems with Magit.

;;;; ___________________________________________________________________________
;;;; ---- -nomis/fix-magit-auto-revert ----
;;;; Don't revert buffers other than after performing a Magit operation that
;;;; changes files.
;;;; See https://emacs.stackexchange.com/questions/35701/magit-sets-auto-revert-mode-annoying

;;;; This is more heavy-handed than I would like. A change to a single file
;;;; (eg by discarding changes to the file) causes all buffers in the repo
;;;; to be reverted.


;;;; FIXME After a stash pop, you have no updates to buffers.

(defun -nomis/fix-magit-auto-revert/2.10.3 ()

  (with-eval-after-load 'magit-autorevert

    (magit-auto-revert-mode 0)

    (defvar *nomis/magit-revert-buffers?* t)

    (defun nomis/magit-refresh ()
      "A replacement for the \"g\" key binding in `magit-mode-map`. This does
not revert buffers."
      (interactive)
      (let* ((*nomis/magit-revert-buffers?* nil))
        (magit-refresh)))
    
    (define-key magit-mode-map "g" 'nomis/magit-refresh)
    
    (advice-add 'magit-auto-revert-buffers
                :around
                (lambda (_)
                  (when *nomis/magit-revert-buffers?*
                    (revert-all-unmodified-buffers-in-git-repo)))
                '((name . -nomis/hack-auto-refresh)))))

(defun -nomis/fix-magit-auto-revert ()
  (cond
   ((member magit-version '("2.10.3"
                            "2.11.0"))
    (-nomis/fix-magit-auto-revert/2.10.3))
   (t
    (message-box (s-join " "
                         '("Revisit `-nomis/fix-magit-auto-revert`"
                           "for this version of Magit."))))))

(add-hook 'magit-mode-hook '-nomis/fix-magit-auto-revert)

;;;; ___________________________________________________________________________

(provide 'nomis-magit-fixes)
