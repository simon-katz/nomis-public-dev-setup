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

(defun _nomis-emacs-lisp-and-ielm-common-setup ()
  (turn-on-elisp-slime-nav-mode)
  (turn-on-eldoc-mode))

(defun nomis-emacs-lisp-setup ()
  (_nomis-emacs-lisp-and-ielm-common-setup)
  (nomis-lispy-non-repl-setup))

(defun nomis-ielm-setup ()
  (_nomis-emacs-lisp-and-ielm-common-setup)
  (nomis-lispy-repl-setup))

;;;; ___________________________________________________________________________

(defun nomis-clojure-setup ()
  (subword-mode))

;;;; ___________________________________________________________________________

(provide 'nomis-lispy-mode-hooks)
