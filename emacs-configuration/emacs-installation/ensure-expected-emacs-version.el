(let ((expected-versions '(;; "27.2"
                           ;; "28.1"
                           ;; "28.2"
                           ;; "29.4"
                           ;; "30.1"
                           "30.2"))
      (version emacs-version))
  (unless (or (member version expected-versions)
              (y-or-n-p
               (format (concat
                        "Things might not work. This Emacs init is"
                        " expecting an Emacs version that is one of %s, but"
                        " this is Emacs %s."
                        " Type 'y' to continue or 'n' to exit.")
                       expected-versions
                       version)))
    (kill-emacs)))

(when (and (equal emacs-version "27.2")
           (eql system-type 'darwin))
  ;; See https://emacs.stackexchange.com/questions/68288/error-retrieving-https-elpa-gnu-org-packages-archive-contents and https://emacs.stackexchange.com/questions/60560/error-retrieving-https-elpa-gnu-org-packages-archive-contents-error-http-400#comment105217_62321
  (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"))
