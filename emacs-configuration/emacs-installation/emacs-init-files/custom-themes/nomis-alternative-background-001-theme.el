(deftheme nomis-alternative-background-001
  "Created 2023-01-03.")

(defconst nomis-alternative-background-001/light-default-background
  "BlanchedAlmond")

(defconst nomis-alternative-background-001/dark-default-background
  "#654020")

(custom-theme-set-faces
 'nomis-alternative-background-001

 `(default
    ((((background light))
      ,(list :background
             nomis-alternative-background-001/light-default-background))
     (((background dark))
      ,(list :background
             nomis-alternative-background-001/dark-default-background))))

 `(auto-dim-other-buffers-face
   ((((background light)) ,(list :background "NavajoWhite1"))
    (((background dark))  ,(list :background "#805737"))))

 `(auto-dim-other-buffers-hide-face
   ((((background light)) ,(list :foreground "NavajoWhite1"
                                 :background "NavajoWhite1"))
    (((background dark))  ,(list :foreground "#805737"
                                 :background "#805737"))))

 `(org-hide
   ((((background light))
     ,(list :foreground
            nomis-alternative-background-001/light-default-background))
    (((background dark))
     ,(list :foreground
            nomis-alternative-background-001/dark-default-background)))))

(provide-theme 'nomis-alternative-background-001)
