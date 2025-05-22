(deftheme nomis-common-dark
  "Created 2023-02-22.")

(custom-theme-set-faces
 'nomis-common-dark

 `(region  ((t  ,(list
                  ;; :foreground "white"
                  :background "Red3"))))

 `(hl-line ((t  ,(list :background "#407040"))))

 `(font-lock-warning-face ((t ,(list :bold t
                                     :foreground "Red2"))))

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
   ((t ,(list :foreground "White"
              :background (cl-case 11
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
                            (10 "DodgerBlue3")
                            (11 "Blue3"))))))

 `(font-lock-comment-face
   ((t  ,(list :foreground "#d0b0ff"))))

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
              :background "Blue1"))))

 `(tldr-title
   ((t ,(list :foreground "Grey90" :height 1.4))))
 `(tldr-introduction
   ((t ,(list :foreground "Grey90"))))
 `(tldr-description
   ((t ,(list :foreground "Grey90"))))
 `(tldr-command-itself
   ((t ,(list :foreground "Black" :background "Green" :weight 'semi-bold))))
 `(tldr-code-block
   ((t ,(list :foreground "Green" :background "Black"))))
 `(tldr-command-argument
   ((t ,(list :foreground "Yellow" :background "Black"))))

 `(cider-fringe-good-face
   ((t ,(list :foreground "DarkGreen" :background "White")))))


(provide-theme 'nomis-common-dark)
