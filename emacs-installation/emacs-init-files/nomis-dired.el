;;;; Init stuff -- Dired.

;;;; ___________________________________________________________________________
;;;; ---- Misc ----

(cond ((equal system-type 'darwin)
       (setq dired-listing-switches "-Ao"))
      ((equal system-type 'windows-nt)
       (message-box "Check dired-listing-switches for windows-nt")))

(require 'nomis-dired-explorer)

;;;; ___________________________________________________________________________

(provide 'nomis-dired)
