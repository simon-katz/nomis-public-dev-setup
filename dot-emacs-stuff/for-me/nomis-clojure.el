;;;; Init stuff -- Clojure mode.

;;;; ___________________________________________________________________________

(require 'clojure-mode)
(require 'cider)

;;;; ___________________________________________________________________________

(dolist (hook '(clojure-mode-hook
                cider-repl-mode-hook))
  (dolist (hook-fun '(rainbow-delimiters-mode
                      paredit-mode
                      subword-mode))
    (add-hook hook hook-fun)))

(define-key clojure-mode-map (kbd "RET") 'newline-and-indent)

;;;; ___________________________________________________________________________
;;;; nrepl

;;;; ---------------------------------------------------------------------------
;;;; ---- Basics ----

;;;; See https://github.com/kingtim/nrepl.el

;;;; ---------------------------------------------------------------------------
;;;; ---- Stuff for the REPL ----

(setq cider-history-file "~/.cider-history")

(setq cider-repl-display-in-current-window t)

;;;; ---------------------------------------------------------------------------
;;;; ---- Stuff for when connected to nrepl       ----
;;;; ---- (for repl and for Clojure source files) ----

(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)

;;;; ___________________________________________________________________________

;;;; TODO: I've seen this a lot. Do I want it?
;;;; (setq nrepl-popup-stacktraces nil)

;;;; TODO: Consider other tailoring.
;;;;       See https://github.com/clojure-emacs/nrepl.el.

;;;; ___________________________________________________________________________

(provide 'nomis-clojure)
