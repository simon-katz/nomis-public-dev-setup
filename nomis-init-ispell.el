;;;; Init stuff -- Ispell

(setq ispell-program-name "/usr/local/bin/ispell")
(setq flyspell-issue-message-flag nil)

(setq flyspell-mark-duplications-exceptions
      (quote ((nil "that" "had" "blah" "plop" "etc")
              ("\\`francais" "nous" "vous"))))

;;;; ___________________________________________________________________________

(provide 'nomis-init-ispell)
