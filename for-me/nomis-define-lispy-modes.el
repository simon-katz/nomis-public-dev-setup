;;;; Init stuff -- Stuff for Lispy modes

;;;; ___________________________________________________________________________

(defun _generic-lispy-stuff-for-both-repls-and-non-repls ()
  (rainbow-delimiters-mode)
  (paredit-mode))

(defun generic-lispy-stuff-for-repls ()
  (_generic-lispy-stuff-for-both-repls-and-non-repls))

(defun generic-lispy-stuff-for-non-repls ()
  (_generic-lispy-stuff-for-both-repls-and-non-repls)
  (whitespace-mode))

;;;; ___________________________________________________________________________

(provide 'nomis-define-lispy-modes)
