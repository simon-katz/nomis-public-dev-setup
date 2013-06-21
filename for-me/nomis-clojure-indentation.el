;;;; Init stuff -- Clojure indentation.

;;;; From https://github.com/weavejester/compojure/wiki/Emacs-indentation.

(require 'clojure-mode)

(define-clojure-indent
  ;; Ring and Compojure
  (defroutes 'defun)
  (GET 2)
  (POST 2)
  (PUT 2)
  (DELETE 2)
  (HEAD 2)
  (ANY 2)
  (context 2)
  ;; Midje
  (fact 1))

;;;; ___________________________________________________________________________

(provide 'nomis-clojure-indentation)
