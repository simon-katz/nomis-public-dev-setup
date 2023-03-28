(deftheme nomis-extras-deeper-blue
  "Created 2022-12-31.")

(require 'color)

(let* ((dark-default-bg         "#181a26") ; the value in `deeper-blue-theme`
       (autodim-dark-default-bg "Grey32"))

  (custom-theme-set-faces
   'nomis-extras-deeper-blue

   `(auto-dim-other-buffers-face
     ((t  ,(list :background autodim-dark-default-bg))))

   `(auto-dim-other-buffers-hide-face
     ((t  ,(list :foreground autodim-dark-default-bg
                 :background autodim-dark-default-bg))))

   `(org-hide
     ((t ,(list :foreground dark-default-bg))))

   ;; The following is based on info at
   ;; https://www.emacswiki.org/emacs/CompanyMode
   `(company-scrollbar-bg
     ((t `(list :background (color-lighten-name autodim-dark-default-bg 40)))))
   `(company-scrollbar-fg
     ((t ,(list :background (color-lighten-name autodim-dark-default-bg 20)))))
   `(company-tooltip-selection
     ((t ,(list :background "Purple4"
                :foreground "PaleTurquoise"))))))

(provide-theme 'nomis-extras-deeper-blue)
