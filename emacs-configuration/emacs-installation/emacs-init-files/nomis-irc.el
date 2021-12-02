;;; nomis-irc.el --- Clojure LSP setup -*- lexical-binding: t; -*-

;;;; ___________________________________________________________________________

(advice-add
 'irc
 :around
 (lambda (orig-fun &rest args)
   (when (y-or-n-p "Really run IRC? That was probably an accident, and you've had Emacs going weird and sometimes crashing before.")
     (apply orig-fun args)))
 '((name . nomis/really-do-irc?)))

;;;; ___________________________________________________________________________

(provide 'nomis-irc)
