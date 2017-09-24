;;;; Init stuff -- Fix problems with Magit.

;;;; ___________________________________________________________________________
;;;; Don't revert buffers other than after performing a Magit operation that
;;;; changes files.
;;;; See https://emacs.stackexchange.com/questions/35701/magit-sets-auto-revert-mode-annoying

;;;; This is more heavy-handed than I would like. A change to a single file
;;;; (eg by discarding changes to the file) causes all buffers in the repo
;;;; to be reverted.

(defun nomis/fix-magit-auto-revert ()
  
  (with-eval-after-load 'magit-autorevert

    (when (equal magit-version "2.10.3")

      (magit-auto-revert-mode 0)

      (defvar *nomis/magit-auto-revert-buffers/do-it?* t)

      (defun nomis/magit-refresh ()
        "A replacement for the \"g\" key binding in `magit-mode-map`. This does
not revert buffers."
        (interactive)
        (let* ((*nomis/magit-auto-revert-buffers/do-it?* nil))
          (magit-refresh)))
      
      (define-key magit-mode-map "g" 'nomis/magit-refresh)
      
      (advice-add 'magit-auto-revert-buffers
                  :around
                  (lambda (_)
                    (when *nomis/magit-auto-revert-buffers/do-it?*
                      (revert-all-unmodified-buffers-in-git-repo)))
                  '((name . nomis/hack-auto-refresh))))))

(add-hook 'magit-mode-hook 'nomis/fix-magit-auto-revert)

;;;; ___________________________________________________________________________

(provide 'nomis-magit-fixes)
