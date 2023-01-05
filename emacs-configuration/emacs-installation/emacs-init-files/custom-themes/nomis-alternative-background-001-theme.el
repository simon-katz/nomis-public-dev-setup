(deftheme nomis-alternative-background-001
  "Created 2023-01-03.")

(custom-theme-set-faces
 'nomis-alternative-background-001

 `(default
    ((((background light)) ,(list :background "BlanchedAlmond"))
     (((background dark))  ,(list :background "#654020"))))

 `(auto-dim-other-buffers-face
   ((((background light)) ,(list :background "NavajoWhite1"))
    (((background dark))  ,(list :background "#805737"))))

 `(auto-dim-other-buffers-hide-face
   ((((background light)) ,(list :foreground "NavajoWhite1"
                                 :background "NavajoWhite1"))
    (((background dark))  ,(list :foreground "#805737"
                                 :background "#805737")))))

(provide-theme 'nomis-alternative-background-001)
