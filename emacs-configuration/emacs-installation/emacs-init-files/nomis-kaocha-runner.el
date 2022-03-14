;;; nomis-kaocha-runner.el --- kaocha-runner setup -*- lexical-binding: t; -*-

;;;; ___________________________________________________________________________

(define-key clojure-mode-map (kbd "C-c k t") 'kaocha-runner-run-test-at-point)
(define-key clojure-mode-map (kbd "C-c k n") 'kaocha-runner-run-tests)
(define-key clojure-mode-map (kbd "C-c k a") 'kaocha-runner-run-all-tests)
(define-key clojure-mode-map (kbd "C-c k w") 'kaocha-runner-show-warnings)
(define-key clojure-mode-map (kbd "C-c k h") 'kaocha-runner-hide-windows)

;;;; ___________________________________________________________________________

(provide 'nomis-kaocha-runner)
