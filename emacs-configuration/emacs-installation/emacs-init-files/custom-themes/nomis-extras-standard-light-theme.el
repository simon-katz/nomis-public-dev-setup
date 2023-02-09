(deftheme nomis-extras-standard-light
  "Created 2022-12-31.")

(custom-theme-set-faces
 'nomis-extras-standard-light

 `(default ((t ,(list :background "WhiteSmoke"))))

 `(auto-dim-other-buffers-face
   ((t ,(list :background "grey84"))))

 `(auto-dim-other-buffers-hide-face
   ((t ,(list :foreground "grey84"
              :background "grey84"))))

 `(hl-line ((t ,(cl-case 0
                  (0 (list :background "darkseagreen1"))
                  (1 (list :background "palegoldenrod"))
                  (2 (list :box (list :line-width -1
                                      :color "grey25"
                                      :style nil)))))))

 `(ido-vertical-first-match-face
   ((t ,(list :foreground "blue"))))

 `(lsp-lens-face
   ((t ,(list :foreground "black"
              :background "DarkSeaGreen2"
              :height 0.8))))

 `(eval-sexp-fu-flash
   ((t  ,(list :background "DodgerBlue"
               :extend     t))))

 `(magit-diff-context-highlight
   ((t ,(list :background "lavender"))))

 `(magit-section-highlight
   ((t ,(list :background (cl-case 5
                            (1 "palegoldenrod")
                            (2 "skyblue")
                            (3 "lightblue")
                            (4 "lightcyan")
                            (5 "lavender")
                            (6 "white"))))))

 `(magit-diff-hunk-heading
   ((t ,(list :foreground "LightBlue2"
              :background "gray20"))))

 `(magit-diff-hunk-heading-highlight
   ((t ,(list :foreground "CadetBlue3"
              :background "gray10")))))


(provide-theme 'nomis-extras-standard-light)
