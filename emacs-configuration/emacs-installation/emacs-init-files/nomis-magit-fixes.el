;;;; Init stuff -- Fix problems with Magit.

;;;; ___________________________________________________________________________
;;;; ---- -nomis/fix-magit-auto-revert ----

;;;; See https://emacs.stackexchange.com/questions/35701/magit-sets-auto-revert-mode-annoying
;;;;
;;;; I've simplified things since the first version of this stuff:
;;;; - Be happy with completely turning off Magit's reverting, even when a
;;;;   Magit operation changes files.
;;;; - Add a keybinding for `nomis/revert-all-unmodified-buffers-in-git-repo`
;;;;   in `magit-mode-map`.
;;;; This will save me thinking each time I upgrade Magit.

(defun -nomis/fix-magit-auto-revert/2.10.3 ()

  (with-eval-after-load 'magit-autorevert

    (magit-auto-revert-mode 0)

    (define-key magit-mode-map "©" ; Option-g on a Mac
      'nomis/revert-all-unmodified-buffers-in-git-repo)))


(defun -nomis/fix-magit-auto-revert ()
  (cond
   ((member magit-version '("2.10.3"
                            "2.11.0"
                            "2.13.0"
                            "2.90.1"
                            "20210810.800"))
    (-nomis/fix-magit-auto-revert/2.10.3))
   (t
    (message-box (s-join " "
                         '("Revisit `-nomis/fix-magit-auto-revert`"
                           "for this version of Magit."))))))


(add-hook 'magit-mode-hook '-nomis/fix-magit-auto-revert)

;;;; ___________________________________________________________________________

(provide 'nomis-magit-fixes)
