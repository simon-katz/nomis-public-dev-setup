(deftheme nomis-extras-dark-laptop
  "Created 2023-02-01.")

(require 'color)

(defconst nomis-extras-dark-laptop/default-background
  "Black")

(defconst nomis-extras-dark-laptop/auto-dim-background
  "Grey32")

(let ((bg nomis-extras-dark-laptop/auto-dim-background))

  (custom-theme-set-faces
   'nomis-extras-dark-laptop

   `(auto-dim-other-buffers-face
     ((t  ,(list :background bg))))

   `(auto-dim-other-buffers-hide-face
     ((t  ,(list :foreground bg
                 :background bg))))

   `(org-hide
     ((t ,(list :foreground nomis-extras-dark-laptop/default-background))))

   ;; The following is based on info at
   ;; https://www.emacswiki.org/emacs/CompanyMode
   `(company-scrollbar-bg
     ((t `(list :background (color-lighten-name bg 40)))))
   `(company-scrollbar-fg
     ((t ,(list :background (color-lighten-name bg 20)))))
   `(company-tooltip-selection
     ((t ,(list :background "Purple4"
                :foreground "PaleTurquoise"))))))

(provide-theme 'nomis-extras-dark-laptop)
