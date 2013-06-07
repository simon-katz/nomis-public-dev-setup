;;;; Init stuff -- slime-fancy.


;; Attempt to get slime-fancy etc

;; - Ah, you need an up-to-date slime (not the one at ELPA).

;; (add-to-list 'load-path "C:/Documents and Settings/Administrator/.emacs.d/elpa/slime-20100404.1")
;; (require 'slime-autoloads)

;; (eval-after-load "slime"
;;   '(progn
;;     (add-to-list 'load-path "D:/downloads/Clojure/slime/slime-2011-03-16/contrib")
;;     (slime-setup '(slime))
;;     ;; (slime-setup '(slime-fancy slime-banner)) ; Symbol's function definition is void: slime-setup-contribs
;;     (slime-setup '(slime-fancy))
;;     (setq slime-complete-symbol*-fancy t)
;;     (setq slime-complete-symbol-function 'slime-fuzzy-complete-symbol)

;;     (setq slime-use-autodoc-mode nil) ; So slime-fancy will work with
;;                                         ; clojure
;;     ))

;;;; ___________________________________________________________________________

(provide 'nomis-slime-fancy)
