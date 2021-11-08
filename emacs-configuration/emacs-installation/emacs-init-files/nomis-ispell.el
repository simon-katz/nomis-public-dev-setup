;;;; Init stuff -- Ispell

(add-hook 'text-mode-hook 'turn-on-flyspell)

(setq ispell-program-name "aspell")

(setq flyspell-issue-message-flag nil)

(setq flyspell-mark-duplications-exceptions
      (quote ((nil "that" "had" "blah" "plop" "etc")
              ("\\`francais" "nous" "vous"))))

;;;; ___________________________________________________________________________

(provide 'nomis-ispell)
