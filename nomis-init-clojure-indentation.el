;;;; Init stuff -- Clojure indentation.

;;;; From https://github.com/weavejester/compojure/wiki/Emacs-indentation.

(require 'clojure-mode)

(define-clojure-indent
  (defroutes 'defun)
  (GET 2)
  (POST 2)
  (PUT 2)
  (DELETE 2)
  (HEAD 2)
  (ANY 2)
  (context 2))
