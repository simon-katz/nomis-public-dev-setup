;;;; Init stuff -- Watch words.

(defvar nomis-watch-words
  '("FIXME"
    "FIX"
    "TODO-THINK"
    "TODO"
    "REMAINING-ISSUE"
    "HACK"
    "REFACTOR"
    "NOCOMMIT"))

(defun nomis-make-regex-from-watchwords-helper (words beginning-of-word-p)
  (with-output-to-string
    (when beginning-of-word-p (princ "\\<"))
    (princ "\\(")
    (princ (first words))
    (dolist (w (rest words))
      (princ "\\|")
      (princ w))
    (princ "\\)")))

(defun nomis-make-regex-from-watchwords/simple ()
  (nomis-make-regex-from-watchwords-helper nomis-watch-words
                                           t))

(defun nomis-make-regex-from-watchwords/bracketed ()
  (let* ((bracketed-watch-words
          (mapcar (lambda (word) (concat "\\[" word "]"))
                  nomis-watch-words)))
    (nomis-make-regex-from-watchwords-helper bracketed-watch-words
                                             nil)))

(defun add-nomis-watch-words ()
  (font-lock-add-keywords
   nil
   `((,(nomis-make-regex-from-watchwords/simple)
      0 font-lock-warning-face t)
     (,(nomis-make-regex-from-watchwords/bracketed)
      0 font-lock-warning-face t))))

(add-hook 'text-mode-hook 'add-nomis-watch-words)
(add-hook 'prog-mode-hook 'add-nomis-watch-words)

;;;; Temp stuff for testing the above:
;;;; - [FIXME]aaa
;;;; - FIXMEaaa
;;;; - [FIX]aaa
;;;; - FIXaaa
;;;; - [TODO-THINK]aaa
;;;; - TODO-THINKaaa
;;;; - [TODO]aaa
;;;; - TODOaaa
;;;; - [HACK]aaa
;;;; - HACKaaa
;;;; - [REFACTOR]aaa
;;;; - REFACTORaaa
;;;; - [NOCOMMIT]aaa
;;;; - NOCOMMITaaa
;;;; - [REMAINING-ISSUE]aaa
;;;; - REMAINING-ISSUEaaa

;;;; ___________________________________________________________________________

(provide 'nomis-watch-words)
