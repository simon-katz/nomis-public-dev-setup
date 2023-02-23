(deftheme nomis-extras-zenburn
  "Created 2022-12-31.")

(defconst nomis-extras-zenburn/default-background
  "#3F3F3F")

(defconst nomis-extras-zenburn/auto-dim-background
  "Grey45")

(custom-theme-set-faces
 'nomis-extras-zenburn

 `(auto-dim-other-buffers-face
   ((t  ,(list :background nomis-extras-zenburn/auto-dim-background))))

 `(auto-dim-other-buffers-hide-face
   ((t  ,(list :foreground nomis-extras-zenburn/auto-dim-background
               :background nomis-extras-zenburn/auto-dim-background))))

 `(org-hide
   ((t ,(list :foreground nomis-extras-zenburn/default-background)))))


(provide-theme 'nomis-extras-zenburn)
