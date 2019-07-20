;;;; Init stuff --- Watch words ---  -*- lexical-binding: t -*-

(defvar nomis/ww/regexps/finger-pointers
  ;; Use \\ in the strings below so you don't get the highlighting here.
  '("-\\-.+--▶"))

(defvar nomis/ww/regexps/high-priority
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
    "L\\EARNING-NOW"))

(defvar nomis/ww/low-priority-regexp-prefix "x")

(defvar nomis/ww/regexps/low-priority
  ;; Use \\ in the strings below so you don't get the highlighting here.
  (append '()
          (mapcar #'(lambda (string)
                      (concat nomis/ww/low-priority-regexp-prefix
                              string))
                  nomis/ww/regexps/high-priority)))

(defface nomis/ww/face/no-priority
  `((t (:foreground ,(case 2
                       (1 "gray27")
                       (2 "DodgerBlue3"))
                    :bold t
                    :italic t
                    )))
  "Face for low priority watch regexps.")

(defface nomis/ww/face/low-priority
  '((t (:foreground "White"
                    :background "Pink4"
                    :bold t
                    :italic t)))
  "Face for low priority watch regexps.")

(defun nomis/ww/ok-to-check-start-of-word? (regexp)
  (not (or (s-starts-with? "-" regexp)
           (s-starts-with? "\\[" regexp)
           ;; This is incomplete!
           )))

(defun nomis/ww/combine-regexps (regexps)
  (cl-flet*
      ((add-start-of-word (regexp)
                          (concat (when (nomis/ww/ok-to-check-start-of-word?
                                         regexp)
                                    "\\<")
                                  regexp))
       (hack-regexp (regexp)
                    (-> regexp
                        add-start-of-word))
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
                          `((,regexp 0 ',face t))))

(defun nomis/ww/add-watches ()
  (cl-loop for (regexps face)
           in `((,nomis/ww/regexps/finger-pointers nomis/ww/face/no-priority)
                (,nomis/ww/regexps/low-priority    nomis/ww/face/low-priority )
                (,nomis/ww/regexps/high-priority   ,font-lock-warning-face))
           do (nomis/ww/add-font-lock (nomis/ww/combine-regexps regexps)
                                      face)))

(add-hook 'text-mode-hook 'nomis/ww/add-watches)
(add-hook 'prog-mode-hook 'nomis/ww/add-watches)

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

;;;; - aaa[--x-y-z--▶]aaa
;;;; - aaa--x-y-z--▶aaa

;;;; ___________________________________________________________________________

(provide 'nomis-watch-words)
