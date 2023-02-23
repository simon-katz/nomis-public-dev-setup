(deftheme nomis-extras-dark-laptop
  "Created 2023-02-01.")

(defconst nomis-extras-dark-laptop/default-background
  "Black")

(defconst nomis-extras-dark-laptop/auto-dim-background
  "Grey32")

(custom-theme-set-faces
 'nomis-extras-dark-laptop

 `(auto-dim-other-buffers-face
   ((t  ,(list :background nomis-extras-dark-laptop/auto-dim-background))))

 `(auto-dim-other-buffers-hide-face
   ((t  ,(list :foreground nomis-extras-dark-laptop/auto-dim-background
               :background nomis-extras-dark-laptop/auto-dim-background))))

 `(org-hide
   ((t ,(list :foreground nomis-extras-dark-laptop/default-background)))))

(provide-theme 'nomis-extras-dark-laptop)
