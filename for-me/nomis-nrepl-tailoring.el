;;;; Init stuff -- nrepl tailoring

;;;; ___________________________________________________________________________
;;;; ---- Basics ----

;;;; See https://github.com/kingtim/nrepl.el

;;;; ---------------------------------------------------------------------------

(when (not (package-installed-p 'nrepl))
  (package-install 'nrepl))


;;;; ---------------------------------------------------------------------------
;;;; ---- Stuff for the REPL ----

(setq nrepl-history-file "~/.nrepl-history.eld")

(add-to-list 'same-window-buffer-names "*nrepl*")

(add-hook 'nrepl-mode-hook 'paredit-mode)
(add-hook 'nrepl-mode-hook 'rainbow-delimiters-mode)
(add-hook 'nrepl-mode-hook 'subword-mode)

;;;; ---------------------------------------------------------------------------
;;;; ---- Stuff for when connected to nrepl       ----
;;;; ---- (for repl and for Clojure source files) ----

(add-hook 'nrepl-interaction-mode-hook 'nrepl-turn-on-eldoc-mode)

;;;; ___________________________________________________________________________

(provide 'nomis-nrepl-tailoring)
