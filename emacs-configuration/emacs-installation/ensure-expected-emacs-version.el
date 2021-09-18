(let ((expected-version "27.1")
      (version emacs-version))
  (unless (or (equal version expected-version)
              (y-or-n-p
               (format (concat
                        "Things might not work. This Emacs init is"
                        " expecting Emacs %s, but this is Emacs %s."
                        " Type 'y' to continue or 'n' to exit.")
                       expected-version
                       version)))
    (kill-emacs)))

(when (and (equal emacs-version "27.2")
           (eql system-type 'darwin))
  ;; See https://emacs.stackexchange.com/questions/68288/error-retrieving-https-elpa-gnu-org-packages-archive-contents and https://emacs.stackexchange.com/questions/60560/error-retrieving-https-elpa-gnu-org-packages-archive-contents-error-http-400#comment105217_62321
  (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"))
