(let (

      ;; BEWARE OF UPGRADING TO Emacs 27.2 -- see https://emacs.stackexchange.com/questions/68288/error-retrieving-https-elpa-gnu-org-packages-archive-contents

      (expected-version "27.1")
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
