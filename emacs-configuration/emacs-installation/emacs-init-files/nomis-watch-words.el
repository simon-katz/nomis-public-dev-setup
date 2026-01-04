;;;; Init stuff --- Watch words ---  -*- lexical-binding: t -*-

(defconst nomis/ww/regexps/finger-pointers
  ;; Use \\ in the strings below so you don't get the highlighting here.
  '("-\\▶"
    "--\\▶"
    "---\\▶"
    "--.*?--\\▶"))

(defconst nomis/ww/regexps/standout
  '("[[:graph:]•]+•"))

(defconst nomis/ww/regexps/operators
  ;; Use \\ in the strings below so you don't get the highlighting here.
  '("--O\\R--"
    "--A\\ND--"
    "--AND/O\\R--"
    "--N\\OT--"
    "--E\\QUIVALENT--"))

(defconst nomis/ww/regexps/high-priority
  ;; Use \\ in the strings below so you don't get the highlighting here.
  '("F\\IXME"
    "F\\IX"
    "T\\ODO-THINK"
    "T\\ODO-with-Picasso-on-site"
    "T\\ODO"
    "T\\O-CHECK"
    "R\\EMAINING-ISSUE"
    "H\\ACK"
    "R\\EFACTOR"
    "N\\OCOMMIT"
    "Q\\UESTION/ACTION"
    "L\\EARNING-NOW"
    "S\\K-COMMENT"))

(defconst nomis/ww/low-priority-regexp-prefix "x")

(defconst nomis/ww/regexps/low-priority
  ;; Use \\ in the strings below so you don't get the highlighting here.
  (append '("N\\OTE")
          (mapcar #'(lambda (string)
                      (concat nomis/ww/low-priority-regexp-prefix
                              string))
                  nomis/ww/regexps/high-priority)))

(defface nomis/ww/face/no-priority
  `((((background dark)) ,(list :foreground "Yellow"
                                :bold t
                                :italic t))
    (t ,(list :foreground "DodgerBlue3"
              :bold t
              :italic t)))
  "Face for no-priority watch regexps.")

(defface nomis/ww/face/standout
  `((((background dark)) ,(list :inherit font-lock-constant-face
                                :slant 'italic))
    (t ,(list :inherit font-lock-constant-face
              :slant 'italic)))
  "Face for standout watch regexps.")

(defface nomis/ww/face/operator
  `((((background dark)) ,(list :foreground "gray85"
                                :bold t
                                :italic t))
    (t ,(list :foreground "gray10"
              :bold t
              :italic t)))
  "Face for operator watch regexps.")

(defface nomis/ww/face/low-priority
  `((((background dark)) ,(list :foreground "Black"
                                :background "Pink2"
                                :bold t
                                :italic t))
    (t ,(list :foreground "White"
              :background "Pink4"
              :bold t
              :italic t)))
  "Face for low-priority watch regexps.")

(defun nomis/ww/check-start-of-symbol? (regexp)
  (not (or (s-starts-with? "-" regexp)
           (s-starts-with? "\\[" regexp)
           ;; This is incomplete!
           )))

(defun nomis/ww/combine-regexps (regexps)
  (cl-flet*
      ((add-start-of-symbol (regexp)
                            (concat (when (nomis/ww/check-start-of-symbol?
                                           regexp)
                                      (nomis/rx/or "\\_<"
                                                   ;; A hack to allow "•" at the
                                                   ;; start. But this will match
                                                   ;; not at start too. What are
                                                   ;; the consequences of this?
                                                   "•"))
                                    regexp))
       (hack-regexp (regexp)
                    (-> regexp
                        add-start-of-symbol))
       (hack-regexps (regexps)
                     (->> regexps
                          (-map #'hack-regexp)
                          (apply #'nomis/rx/or)))
       (wrap-with-brackets (r)
                           (concat "\\[" r "\\]")))
    (nomis/rx/or (hack-regexps (-map #'wrap-with-brackets regexps))
                 (hack-regexps regexps))))

(defun nomis/ww/add-font-lock (regexp face)
  (font-lock-add-keywords nil
                          `((,regexp 0 ',face prepend))
                          ;; Adding this `add-at-end` arg means we don't blat
                          ;; header styling in org mode, so I guess it's the
                          ;; right thing to be doing in general.
                          'add-at-end))

(defun nomis/ww/add-watches ()
  (cl-loop for (regexps face)
           in `((,nomis/ww/regexps/finger-pointers nomis/ww/face/no-priority)
                (,nomis/ww/regexps/standout        nomis/ww/face/standout)
                (,nomis/ww/regexps/operators       nomis/ww/face/operator)
                (,nomis/ww/regexps/low-priority    nomis/ww/face/low-priority )
                (,nomis/ww/regexps/high-priority   ,font-lock-warning-face))
           do (nomis/ww/add-font-lock (nomis/ww/combine-regexps regexps)
                                      face)))

(add-hook 'text-mode-hook 'nomis/ww/add-watches)
(add-hook 'prog-mode-hook 'nomis/ww/add-watches)

(when nil ; for dev
  (progn
    (remove-hook 'text-mode-hook 'nomis/ww/add-watches)
    (remove-hook 'prog-mode-hook 'nomis/ww/add-watches)))

;;;; ___________________________________________________________________________
;;;; Stuff for testing the above:

;;;; - [QUESTION/ACTION]aaa
;;;; - QUESTION/ACTIONaaa
;;;; - [FIXME]aaa
;;;; - FIXMEaaa
;;;; - [FIX]aaa
;;;; - FIXaaa
;;;; - [TODO-THINK]aaa
;;;; - TODO-THINKaaa
;;;; - [TODO]aaa
;;;; - TODOaaa
;;;; - [TODO-with-Picasso-on-site]aaa
;;;; - TODO-with-Picasso-on-siteaaa
;;;; - [HACK]aaa
;;;; - HACKaaa
;;;; - [REFACTOR]aaa
;;;; - REFACTORaaa
;;;; - [NOCOMMIT]aaa
;;;; - NOCOMMITaaa
;;;; - [REMAINING-ISSUE]aaa
;;;; - REMAINING-ISSUEaaa
;;;; - aaaREMAINING-ISSUEaaa   ---- no highlighting

;;;; - [xQUESTION/ACTION]aaa
;;;; - xQUESTION/ACTIONaaa
;;;; - [xFIXME]aaa
;;;; - xFIXMEaaa
;;;; - [xFIX]aaa
;;;; - xFIXaaa
;;;; - [xTODO-THINK]aaa
;;;; - xTODO-THINKaaa
;;;; - [xTODO]aaa
;;;; - xTODOaaa
;;;; - [xTODO-with-Picasso-on-site]aaa
;;;; - xTODO-with-Picasso-on-siteaaa
;;;; - [xHACK]aaa
;;;; - xHACKaaa
;;;; - [xREFACTOR]aaa
;;;; - xREFACTORaaa
;;;; - [xNOCOMMIT]aaa
;;;; - xNOCOMMITaaa
;;;; - [xREMAINING-ISSUE]aaa
;;;; - xREMAINING-ISSUEaaa
;;;; - aaaxREMAINING-ISSUEaaa   ---- no highlighting

;;;; - aaa[-▶]aaa
;;;; - aaa-▶aaa
;;;; - aaa[--▶]aaa
;;;; - aaa--▶aaa
;;;; - aaa[---▶]aaa
;;;; - aaa---▶aaa
;;;; - aaa[----▶]aaa
;;;; - aaa----▶aaa
;;;; - aaa[--x-y-z--▶]aaa
;;;; - aaa--x-y-z--▶aaa
;;;; - aaa[-▶]aaa aaa[-▶]aaa

;;;; - •abc•
;;;; - •abc•s
;;;; - •abc•s and •def•
;;;; - a•bc• and a•bc•s and a•bc•def• and a•bc•def•s
;;;; - (and (• a b) (• d e)) ; Not caught by •...•.

;;;; - aaa[--OR--]aaa
;;;; - aaa--OR--aaa
;;;; - aaa[--AND--]aaa
;;;; - aaa--AND--aaa
;;;; - aaa[--AND/OR--]aaa
;;;; - aaa--AND/OR--aaa
;;;; - aaa[--NOT--]aaa
;;;; - aaa--NOT--aaa
;;;; - aaa[--EQUIVALENT--]aaa
;;;; - aaa--EQUIVALENT--aaa

;;;; ___________________________________________________________________________

(provide 'nomis-watch-words)
