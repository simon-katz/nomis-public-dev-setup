;;;; Init stuff -- Watch-words.

(defun add-nomis-watch-words ()
  (font-lock-add-keywords
   nil
   '(("\\[REMAINING-ISSUE\\]"
      0 font-lock-warning-face t)
     ("\\<\\(REMAINING-ISSUE\\):"
      1 font-lock-warning-face t)
     ("\\<\\(TOD\O-THINK\\):"
      1 font-lock-warning-face t))))

;; I'm getting:
;;   emacs file mode specification error void-function add-watchwords
;; (add-hook 'text-mode-hook 'add-watchwords) ; does (font-lock-add-keywords nil '(("\\<\\(FIX\\|TOD\O\\|FIXME\\|HACK\\|REFACTOR\\):" 1 font-lock-warning-face t)))

(add-hook 'text-mode-hook 'add-nomis-watch-words)
(add-hook 'coding-hook    'add-nomis-watch-words)

;;;; Temp stuff for testing the above:  FIX: Seems not to be working. Compare with Windows installation.
;;;; - FIXaaa
;;;; - TODOaaa
;;;; - FIXMEaaa
;;;; - HACKaaa
;;;; - REFACTORaaa
;;;; - [REMAINING-ISSUE]aaa
;;;; - REMAINING-ISSUEaaa
;;;; - TODO-THINKaaa

;;;; ___________________________________________________________________________

(provide 'nomis-watch-words)
