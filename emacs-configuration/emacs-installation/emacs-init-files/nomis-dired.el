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
  (let* ((gls (-first #'file-exists-p
                      '("/opt/homebrew/bin/gls"
                        "/usr/local/bin/gls"))))
    (when gls
      (setq insert-directory-program gls)
      (setq dired-listing-switches
            (concatenate 'string
                         dired-listing-switches
                         " "
                         "--time-style long-iso")))))

(when (equal system-type 'windows-nt)
  (message-box "Check dired setup for windows-nt"))

;;;; ___________________________________________________________________________

(require 'nomis-dired-explorer)

;;;; ___________________________________________________________________________

(provide 'nomis-dired)
