(deftheme nomis-alternative-background-002
  "Created 2023-01-03.")

(defconst nomis-alternative-background-002/light-default-background
  "MistyRose")

(defconst nomis-alternative-background-002/dark-default-background
  "#603020")

(custom-theme-set-faces
 'nomis-alternative-background-002

 `(default
    ((((background light))
      ,(list :background
             nomis-alternative-background-002/light-default-background))
     (((background dark))
      ,(list :background
             nomis-alternative-background-002/dark-default-background))))

 `(auto-dim-other-buffers-face
   ((((background light)) ,(list :background "MistyRose2"))
    (((background dark))  ,(list :background "#805040"))))

 `(auto-dim-other-buffers-hide-face
   ((((background light)) ,(list :foreground "MistyRose2"
                                 :background "MistyRose2"))
    (((background dark))  ,(list :foreground "#805040"
                                 :background "#805040"))))

 `(org-hide
   ((((background light))
     ,(list :foreground
            nomis-alternative-background-002/light-default-background))
    (((background dark))
     ,(list :foreground
            nomis-alternative-background-002/dark-default-background)))))

(provide-theme 'nomis-alternative-background-002)
