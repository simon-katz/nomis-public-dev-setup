(deftheme nomis-extras-dark-laptop
  "Created 2023-02-01.")

(defconst nomis-extras-dark-laptop/default-background
  "Black")

(custom-theme-set-faces
 'nomis-extras-dark-laptop

 `(region  ((t  ,(list :foreground "white"
                       :background "DodgerBlue4"))))

 `(hl-line ((t  ,(list :background "grey20"))))

 `(font-lock-comment-face
   ((t  ,(list :foreground "PaleVioletRed1"))))

 `(eval-sexp-fu-flash
   ((t  ,(list :background "DodgerBlue3"
               :extend     t))))

 `(org-hide
   ((t ,(list :foreground nomis-extras-dark-laptop/default-background)))))

(provide-theme 'nomis-extras-dark-laptop)
