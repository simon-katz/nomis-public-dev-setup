;;;; nomis-electric-clojure-tailoring.el --- nomis-electric-clojure-mode tailoring ---  -*- lexical-binding: t -*-

;;;; ___________________________________________________________________________

(define-key clojure-mode-map (kbd "M-E") 'nomis-electric-clojure-mode)
(define-key clojure-mode-map (kbd "C-M-e") 'nomis/ec-cycle-options)

;;;; ___________________________________________________________________________

(provide 'nomis-electric-clojure-tailoring)
