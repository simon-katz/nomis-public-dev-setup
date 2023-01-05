(deftheme nomis
  "Created 2022-12-31.")

(custom-theme-set-faces
 'nomis

 `(default ((((background light)) ,(list :background "WhiteSmoke"))))

 `(auto-dim-other-buffers-face
   ((((background light)) ,(list :background "grey84"))
    (((background dark))  ,(list :background "#303030"))))

 `(auto-dim-other-buffers-hide-face
   ((((background light)) ,(list :foreground "grey84"
                                 :background "grey84"))
    (((background dark))  ,(list :foreground "#303030"
                                 :background "#303030"))))

 `(region  ((((background dark))  ,(list :foreground "white"
                                         :background "DodgerBlue4"))))

 `(hl-line ((((background light)) ,(cl-case 0
                                     (0 (list :background "darkseagreen1"))
                                     (1 (list :background "palegoldenrod"))
                                     (2 (list :box (list :line-width -1
                                                         :color "grey25"
                                                         :style nil)))))
            (((background dark))  ,(list :background "grey20"))))

 `(ido-vertical-first-match-face
   ((((background light)) ,(list :foreground "blue"))))

 `(lsp-lens-face
   ((default ,(list :height 0.8))
    (((background light)) ,(list :foreground "black"
                                 :background "DarkSeaGreen2"))
    (((background dark))  ,(list :foreground "yellow"
                                 :background "blue4"))))

 `(font-lock-comment-face
   ((((background dark))  ,(list :foreground "PaleVioletRed1"))))

 `(magit-diff-context-highlight
   ((((background light)) ,(list :background "lavender"))
    (((background dark)) ,(list :background "#222240"))))

 `(magit-section-highlight
   ((((background light)) ,(list :background (cl-case 5
                                               (1 "palegoldenrod")
                                               (2 "skyblue")
                                               (3 "lightblue")
                                               (4 "lightcyan")
                                               (5 "lavender")
                                               (6 "white"))))
    (((background dark)) ,(list :background "#222244"))))

 `(magit-diff-hunk-heading
   ((((background light)) ,(list :foreground "LightBlue2"
                                 :background "gray20"))
    (((background dark)) ,(list :foreground "gray80"
                                :background "Blue4"))))

 `(magit-diff-hunk-heading-highlight
   ((((background light)) ,(list :foreground "CadetBlue3"
                                 :background "gray10"))
    (((background dark)) ,(list :foreground "gray80"
                                :background "Blue1")))))


(provide-theme 'nomis)
