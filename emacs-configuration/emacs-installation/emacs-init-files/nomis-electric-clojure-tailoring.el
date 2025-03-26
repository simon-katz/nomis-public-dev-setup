;;;; nomis-electric-clojure-tailoring.el --- nomis-electric-clojure-mode tailoring ---  -*- lexical-binding: t -*-

;;;; ___________________________________________________________________________
;;;; Make background colors more subtle.

(face-spec-set 'nomis/ec-client-face/using-background
  `((((background dark)) ,(list :background "#005500"))
    (t ,(list :background "DarkSeaGreen1"))))

(face-spec-set 'nomis/ec-server-face/using-background
  `((((background dark)) ,(list :background "#700000"))
    (t ,(list :background "#ffc5c5"))))

;;;; ___________________________________________________________________________

(define-key clojure-mode-map (kbd "M-E") 'nomis-electric-clojure-mode)
(define-key clojure-mode-map (kbd "C-M-e") 'nomis/ec-cycle-options)

;;;; ___________________________________________________________________________

(add-hook 'nomis/themes/theme-changed-hook
          'nomis/ec-update-normal-neutral-face)

;;;; ___________________________________________________________________________

(provide 'nomis-electric-clojure-tailoring)
