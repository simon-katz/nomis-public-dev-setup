(deftheme nomis-extras-dark-laptop
  "Created 2023-02-01.")

(custom-theme-set-faces
 'nomis-extras-dark-laptop

 `(region  ((t  ,(list :foreground "white"
                       :background "DodgerBlue4"))))

 `(hl-line ((t  ,(list :background "grey20"))))

 `(font-lock-comment-face
   ((t  ,(list :foreground "PaleVioletRed1")))))


(provide-theme 'nomis-extras-dark-laptop)