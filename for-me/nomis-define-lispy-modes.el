;;;; Init stuff -- Stuff for Lispy modes

;;;; ___________________________________________________________________________

(defun _nomis-lispy-common-repl-and-non-repl-setup ()
  (rainbow-delimiters-mode)
  (paredit-mode))

(defun nomis-lispy-repl-setup ()
  (_nomis-lispy-common-repl-and-non-repl-setup))

(defun nomis-lispy-non-repl-setup ()
  (_nomis-lispy-common-repl-and-non-repl-setup)
  (whitespace-mode))

;;;; ___________________________________________________________________________

(provide 'nomis-define-lispy-modes)
