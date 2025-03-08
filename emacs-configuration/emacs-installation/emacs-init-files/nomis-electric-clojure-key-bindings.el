;;;; nomis-electric-clojure-key-bindings.el --- Electric Clojure key bindings ---  -*- lexical-binding: t -*-

;;;; ___________________________________________________________________________

(define-key clojure-mode-map (kbd "M-E") 'nomis-electric-clojure-mode)

(define-key clojure-mode-map (kbd "C-M-E")
            'nomis/ec-toggle-highlight-initial-whitespace?)

;;;; ___________________________________________________________________________

(provide 'nomis-electric-clojure-key-bindings)
