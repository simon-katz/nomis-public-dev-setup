(deftheme nomis-extras-zenburn
  "Created 2022-12-31.")

(let* ((dark-default-bg         "#3F3F3F") ; the value in `zenburn-theme`
       (autodim-dark-default-bg "Grey45"))

  (custom-theme-set-faces
   'nomis-extras-zenburn

   `(auto-dim-other-buffers-face
     ((t  ,(list :background autodim-dark-default-bg))))

   `(auto-dim-other-buffers-hide-face
     ((t  ,(list :foreground autodim-dark-default-bg
                 :background autodim-dark-default-bg))))

   `(org-hide
     ((t ,(list :foreground dark-default-bg))))))


(provide-theme 'nomis-extras-zenburn)
