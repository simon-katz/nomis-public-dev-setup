;;;; Init stuff -- Clojure mode.

;;;; ___________________________________________________________________________

(require 'clojure-mode)
(require 'cider)

(require 'nomis-clojure-indentation)
(require 'nomis-cider-extras)

(require 'align-cljlet)

;;;; ___________________________________________________________________________
;;;; clj-refactor

(require 'clj-refactor)

(defun nomis-setup-clj-refactor-mode ()
  (clj-refactor-mode 1)
  (cljr-add-keybindings-with-prefix "M-R"))

(add-hook 'clojure-mode-hook (lambda () (yas/minor-mode 1)))

;;;; ___________________________________________________________________________

(dolist (hook '(clojure-mode-hook
                cider-repl-mode-hook))
  (dolist (hook-fun '(rainbow-delimiters-mode
                      paredit-mode
                      subword-mode
                      nomis-setup-clj-refactor-mode))
    (add-hook hook hook-fun)))

(define-key clojure-mode-map (kbd "RET") 'newline-and-indent)

;;;; ___________________________________________________________________________
;;;; Cider

;;;; See https://github.com/clojure-emacs/cider.

;;;; ---------------------------------------------------------------------------
;;;; ---- Stuff for the REPL ----

(setq nrepl-buffer-name-separator "--")

;; (setq nrepl-buffer-name-show-port t)
(setq cider-repl-display-in-current-window t)
(setq cider-repl-print-length 100)

;; (setq cider-repl-history-file "~/.cider-repl-history") ; I'm now hacking this in `cider-jack-in`
(setq cider-repl-history-size 5000) ; the default is 500

;; (setq cider-repl-use-pretty-printing t) ; doesn't work with ClojureScript

(setq cider-repl-use-clojure-font-lock t)

;; ;;;; ---------------------------------------------------------------------------
;; ;;;; ---- Stuff for when connected to nrepl       ----
;; ;;;; ---- (for REPL and for Clojure source files) ----

(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)


;; ;;;; ---------------------------------------------------------------------------
;; ;;;; ---- Stack backtraces ----

;;;; TODO: I've seen this a lot. Do I want it?
;; (setq cider-popup-stacktraces nil)
;; (setq cider-repl-popup-stacktraces t)
;; (setq cider-auto-select-error-buffer t)


;;;; ___________________________________________________________________________

(provide 'nomis-clojure)
