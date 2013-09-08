;;;; Init stuff -- Clojure mode.

;;;; TODO: Get rid of `nomis-nrepl-tailoring` and move contents to here.

;;;; ___________________________________________________________________________

(require 'clojure-mode)
(require 'nrepl)

(dolist (hook '(clojure-mode-hook
                nrepl-mode-hook))
  (dolist (hook-fun '(rainbow-delimiters-mode
                      paredit-mode
                      subword-mode))
    (add-hook hook hook-fun)))

(define-key clojure-mode-map (kbd "RET") 'newline-and-indent)

;;;; ___________________________________________________________________________

(provide 'nomis-clojure)
