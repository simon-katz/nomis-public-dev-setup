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
;;;; Cider

;;;; See https://github.com/clojure-emacs/cider.
;;;; - TODO: Consider other tailoring.

;;;; ---------------------------------------------------------------------------
;;;; ---- Stuff for the REPL ----

(setq nrepl-buffer-name-separator "--")
(setq cider-repl-display-in-current-window t)

(setq cider-history-file "~/.cider-history")
(setq cider-history-size 1000) ; the default is 500

(setq cider-repl-use-pretty-printing t)

;;;; ---------------------------------------------------------------------------
;;;; ---- Stuff for when connected to nrepl       ----
;;;; ---- (for REPL and for Clojure source files) ----

(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)

;;;; ---------------------------------------------------------------------------
;;;; ---- Stack backtraces ----

(setq cider-repl-popup-stacktraces t)
(setq cider-auto-select-error-buffer t)

;;;; ___________________________________________________________________________

(provide 'nomis-clojure)
