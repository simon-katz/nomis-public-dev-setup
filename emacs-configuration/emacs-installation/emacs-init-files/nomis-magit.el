;;;; Init stuff -- nomis-magit  -*- lexical-binding: t; -*-

;;;; ___________________________________________________________________________
;;;; ---- -nomis/polite-revert ----

;;;; See https://emacs.stackexchange.com/questions/35701/magit-sets-auto-revert-mode-annoying
;;;;
;;;; Don't globally set auto-revert-mode (that's very rude!). Instead, revert
;;;; buffers in the repo after each Magit operation.

(magit-auto-revert-mode 0)

;;;; TODO: I'm not sure, but maybe Magit no longer sets auto-revert-mode
;;;;       for buffers, and maybe it just reverts them.
;;;;       In which case your advice is more-or-less just turning on
;;;;       magit-auto-revert-mode (except you only revert unmodified buffers).
;;;;       Try this out tomorrow.

(with-eval-after-load 'magit-mode
  (cond
   ((member (magit-version) '("20210913.1931"))
    (advice-add
     'magit-auto-revert-buffers
     :around
     (lambda (_orig-fun &rest _args)
       (nomis/polite-revert/auto-revert))
     '((name . -nomis/polite-revert))))
   (t
    (message-box (s-join " "
                         '("Revisit `-nomis/polite-revert`"
                           "for this version of Magit."))))))

(provide 'nomis-magit)
