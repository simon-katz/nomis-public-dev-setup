;;;; Init stuff -- Clojure mode.

;;;; ___________________________________________________________________________

(require 'clojure-mode)
(require 'nrepl) ; xxxx-cider cider

;;;; ___________________________________________________________________________

(dolist (hook '(clojure-mode-hook
                nrepl-mode-hook)) ; xxxx-cider cider-repl-mode-hook
  (dolist (hook-fun '(rainbow-delimiters-mode
                      paredit-mode
                      subword-mode))
    (add-hook hook hook-fun)))

(define-key clojure-mode-map (kbd "RET") 'newline-and-indent)

;;;; ___________________________________________________________________________
;;;; nrepl ; xxxx-cider Cider

;;;; See https://github.com/clojure-emacs/cider.
;;;; - TODO: Consider other tailoring.

;;;; ---------------------------------------------------------------------------
;;;; ---- Stuff for the REPL ----

;; (setq nrepl-buffer-name-separator "--") ; xxxx-cider
(add-to-list 'same-window-buffer-names "*nrepl*") ; (setq cider-repl-display-in-current-window t) ; xxxx-cider

(setq nrepl-history-file "~/.nrepl-history.eld") ; xxxx-cider (setq cider-history-file "~/.cider-history")
;; (setq cider-history-size 1000) ; the default is 500 ; xxxx-cider

;; (setq cider-repl-use-pretty-printing t) ; xxxx-cider

;;;; ---------------------------------------------------------------------------
;;;; ---- Stuff for when connected to nrepl       ----
;;;; ---- (for REPL and for Clojure source files) ----

(add-hook 'nrepl-interaction-mode-hook 'nrepl-turn-on-eldoc-mode) ; xxxx-cider (add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)

;;;; ---------------------------------------------------------------------------
;;;; ---- Stack backtraces ----

;;;; TODO: I've seen this a lot. Do I want it?
;;;; (setq nrepl-popup-stacktraces nil)

;; (setq cider-repl-popup-stacktraces t) ; xxxx-cider
;; (setq cider-auto-select-error-buffer t) ; xxxx-cider

;;;; ___________________________________________________________________________

(provide 'nomis-clojure)
