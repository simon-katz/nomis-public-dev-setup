(deftheme nomis-extras-deeper-blue
  "Created 2022-12-31.")

(custom-theme-set-faces
 'nomis-extras-deeper-blue

 `(auto-dim-other-buffers-face
   ((t  ,(list :background "#303030"))))

 `(auto-dim-other-buffers-hide-face
   ((t  ,(list :foreground "#303030"
               :background "#303030"))))

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


(provide-theme 'nomis-extras-deeper-blue)
