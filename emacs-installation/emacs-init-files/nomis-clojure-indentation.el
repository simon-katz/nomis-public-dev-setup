;;;; Init stuff -- Clojure indentation.

(eval-after-load 'clojure-mode
  '(define-clojure-indent
     ;; Clojure core
     (cond-> 1)
     ;; Ring and Compojure
     ;; From https://github.com/weavejester/compojure/wiki/Emacs-indentation.
     (defroutes 'defun)
     (GET 2)
     (POST 2)
     (PUT 2)
     (DELETE 2)
     (HEAD 2)
     (ANY 2)
     (context 2)
     ;; Midje
     (fact 'defun)
     (facts 'defun)
     (against-background 'defun)
     (provided 0)))

;;;; ___________________________________________________________________________

(provide 'nomis-clojure-indentation)
