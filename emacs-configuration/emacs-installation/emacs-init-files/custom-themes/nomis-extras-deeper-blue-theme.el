(deftheme nomis-extras-deeper-blue
  "Created 2022-12-31.")

(defconst nomis-extras-deeper-blue/default-background
  "#181a26")

(defconst nomis-extras-deeper-blue/auto-dim-background
  "Grey32")

(custom-theme-set-faces
 'nomis-extras-deeper-blue

 `(auto-dim-other-buffers-face
   ((t  ,(list :background nomis-extras-deeper-blue/auto-dim-background))))

 `(auto-dim-other-buffers-hide-face
   ((t  ,(list :foreground nomis-extras-deeper-blue/auto-dim-background
               :background nomis-extras-deeper-blue/auto-dim-background))))

 `(org-hide
   ((t ,(list :foreground nomis-extras-deeper-blue/default-background)))))

(provide-theme 'nomis-extras-deeper-blue)
