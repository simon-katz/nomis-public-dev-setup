(deftheme nomis-extras-zenburn
  "Created 2022-12-31.")

(custom-theme-set-faces
 'nomis-extras-zenburn

 `(auto-dim-other-buffers-face
   ((t  ,(list :background "#505050"))))

 `(auto-dim-other-buffers-hide-face
   ((t  ,(list :foreground "#505050"
               :background "#505050"))))

 `(region  ((t  ,(list :foreground "white"
                       :background "DodgerBlue4"))))

 `(hl-line ((t  ,(list :background "grey20"))))

 `(lsp-lens-face
   ((t  ,(list :foreground "yellow"
               :background "blue4"
               :height 0.8))))

 `(font-lock-comment-face
   ((t  ,(list :foreground "PaleVioletRed1"))))

 `(magit-diff-context-highlight
   ((t ,(list :background "#222240"))))

 `(magit-section-highlight
   ((t ,(list :background "#222244"))))

 `(magit-diff-hunk-heading
   ((t ,(list :foreground "gray80"
              :background "Blue4"))))

 `(magit-diff-hunk-heading-highlight
   ((t ,(list :foreground "gray80"
              :background "Blue1")))))


(provide-theme 'nomis-extras-zenburn)