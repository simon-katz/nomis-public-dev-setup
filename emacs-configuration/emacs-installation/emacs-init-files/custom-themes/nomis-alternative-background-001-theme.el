(deftheme nomis-alternative-background-001
  "Created 2023-01-03.")

(let* ((light-default-bg         "BlanchedAlmond")
       (dark-default-bg          "#654020")
       (autodim-light-default-bg "NavajoWhite1")
       (autodim-dark-default-bg  "#805737"))

  (custom-theme-set-faces
   'nomis-alternative-background-001

   `(default
      ((((background light)) ,(list :background light-default-bg))
       (((background dark))  ,(list :background dark-default-bg))))

   `(auto-dim-other-buffers-face
     ((((background light)) ,(list :background autodim-light-default-bg))
      (((background dark))  ,(list :background autodim-dark-default-bg))))

   `(auto-dim-other-buffers-hide-face
     ((((background light)) ,(list :foreground autodim-light-default-bg
                                   :background autodim-light-default-bg))
      (((background dark))  ,(list :foreground autodim-dark-default-bg
                                   :background autodim-dark-default-bg))))

   `(org-hide
     ((((background light)) ,(list :foreground light-default-bg))
      (((background dark))  ,(list :foreground dark-default-bg))))))

(provide-theme 'nomis-alternative-background-001)
