(let ((expected-version "24.3.1")
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
