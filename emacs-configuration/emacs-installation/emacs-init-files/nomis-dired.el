;;;; Init stuff -- Dired.

;;;; ___________________________________________________________________________
;;;; ---- Misc ----

(setq dired-listing-switches
      (concatenate 'string
                   "-"
                   "a" ; show dotfiles
                   "o" ; like -l but without group information
                   "h" ; human-readable
                   ))

(when (equal system-type 'darwin)
  (let ((gls "/usr/local/bin/gls"))
    (when (file-exists-p gls)
      (setq insert-directory-program gls))))

(when (equal system-type 'windows-nt)
  (message-box "Check dired setup for windows-nt"))

;;;; ___________________________________________________________________________

(require 'nomis-dired-explorer)

;;;; ___________________________________________________________________________

(provide 'nomis-dired)
