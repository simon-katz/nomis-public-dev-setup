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

 `(font-lock-comment-face
   ((t  ,(list :foreground "PaleVioletRed1"))))

 `(eval-sexp-fu-flash
   ((t  ,(list :background "DodgerBlue3"
               :extend     t))))

 `(magit-section-highlight ((t  ,(list :background "DodgerBlue4"))))

 `(magit-diff-context-highlight
   ((t ,(list :background "#222240"))))

 `(magit-section-highlight
   ((t ,(list :background "#222244"))))

 `(magit-diff-hunk-heading
   ((t ,(list :foreground "gray80"
              :background "Blue4"))))

 `(magit-diff-hunk-heading-highlight
   ((t ,(list :foreground "gray80"
              :background "Blue1")))))


(provide-theme 'nomis-common-dark)
