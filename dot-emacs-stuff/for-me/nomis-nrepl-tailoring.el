;;;; Init stuff -- nrepl tailoring

(require 'nrepl)
(require 'nomis-lispy-mode-hooks)

;;;; ___________________________________________________________________________
;;;; ---- Basics ----

;;;; See https://github.com/kingtim/nrepl.el

;;;; ---------------------------------------------------------------------------
;;;; ---- Stuff for the REPL ----

(setq nrepl-history-file "~/.nrepl-history.eld")

(add-to-list 'same-window-buffer-names "*nrepl*")

;;;; ---------------------------------------------------------------------------
;;;; ---- Stuff for when connected to nrepl       ----
;;;; ---- (for repl and for Clojure source files) ----

(add-hook 'nrepl-interaction-mode-hook 'nrepl-turn-on-eldoc-mode)

;;;; ___________________________________________________________________________

;;;; TODO: I've seen this a lot. Do I want it?
;;;; (setq nrepl-popup-stacktraces nil)

;;;; TODO: Consider other tailoring.
;;;;       See https://github.com/clojure-emacs/nrepl.el.

;;;; ___________________________________________________________________________

(provide 'nomis-nrepl-tailoring)
