;;;; nomis-electric-clojure-tailoring.el --- nomis-electric-clojure-mode tailoring ---  -*- lexical-binding: t -*-

;;;; ___________________________________________________________________________

(define-key clojure-mode-map (kbd "M-E") 'nomis-electric-clojure-mode)

(define-key clojure-mode-map (kbd "C-M-E")
            'nomis/ec-toggle-color-initial-whitespace)

;;;; ___________________________________________________________________________

(provide 'nomis-electric-clojure-tailoring)
