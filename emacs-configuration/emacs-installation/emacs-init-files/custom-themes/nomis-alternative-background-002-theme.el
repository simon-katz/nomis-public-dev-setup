(deftheme nomis-alternative-background-002
  "Created 2023-01-03.")

(custom-theme-set-faces
 'nomis-alternative-background-002

 `(default
    ((((background light)) ,(list :background "MistyRose"))
     (((background dark))  ,(list :background "#603020"))))

 `(auto-dim-other-buffers-face
   ((((background light)) ,(list :background "MistyRose2"))
    (((background dark))  ,(list :background "#805040"))))

 `(auto-dim-other-buffers-hide-face
   ((((background light)) ,(list :foreground "MistyRose2"
                                 :background "MistyRose2"))
    (((background dark))  ,(list :foreground "#805040"
                                 :background "#805040")))))

(provide-theme 'nomis-alternative-background-002)
