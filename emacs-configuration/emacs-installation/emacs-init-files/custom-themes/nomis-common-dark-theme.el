(deftheme nomis-common-dark
  "Created 2023-02-22.")

(custom-theme-set-faces
 'nomis-common-dark

 `(region  ((t  ,(list
                  ;; :foreground "white"
                  :background "DodgerBlue4"))))

 `(hl-line ((t  ,(list :background "#306030"))))

 `(lsp-lens-face
   ((t  ,(list :foreground "yellow"
               :background "blue4"
               :height 0.8))))

 `(lsp-ui-sideline-global
   ((t
     :background "Red4")))

 `(lsp-ui-sideline-code-action
   ((t
     :foreground "gray95"
     :background "springgreen4")))

 `(lsp-face-highlight-textual
   ;; c.f. tailoring of `nomis/idle-highlight-muted`.
   ((t ,(list :background (cl-case 3
                            (-1 "Blue4")
                            (0 "Blue1")
                            (1 "Purple") ; #942092
                            (2 "#801080")
                            (3 "DeepPink3")
                            (4 "tomato4")
                            (5 "tan3")
                            (6 "sienna3")
                            (7 "DeepSkyBlue4") ; #00688b
                            (8 "#007090")
                            (9 "IndianRed")
                            (10 "DodgerBlue3"))))))

 `(hc-hard-space
   ((t
     :background "grey70")))

 `(font-lock-comment-face
   ((t  ,(list :foreground "PaleVioletRed1"))))

 `(eval-sexp-fu-flash
   ((t  ,(list :background "DodgerBlue3"
               :extend     t))))

 `(magit-section-highlight ((t  ,(list :background "DarkCyan"))))

 `(magit-diff-context-highlight
   ((t ,(list :background "#222240"))))

 `(magit-diff-hunk-heading
   ((t ,(list :foreground "gray80"
              :background "Blue4"))))

 `(magit-diff-hunk-heading-highlight
   ((t ,(list :foreground "gray80"
              :background "Blue1")))))


(provide-theme 'nomis-common-dark)
