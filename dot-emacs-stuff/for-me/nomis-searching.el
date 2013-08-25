;;;; Init stuff -- Searching.

;;;; ___________________________________________________________________________
;;;; ---- Generic grep stuff ----

(progn
  ;; M-> and M-< are by default bound to end-of-buffer and
  ;; beginning-of-buffer, which are also bound to C-end and C-home.
  ;; Rebind M-> and M-< without much lossage.
  ;; TODO: These bindings are getting lost and replaced with the defaults.
  ;;       - Do you need this in a hook function?  (What about other uses
  ;;         of `define-key`?
  (define-key global-map (kbd "M->") 'next-error)
  (define-key global-map (kbd "M-<") 'previous-error))

;;;; ___________________________________________________________________________
;;;; ---- Stuff for rgrep and lgrep ----

(defadvice grep-tag-default (around change-space-to-todo ()) ; TODO: Huh? What's this for?
  (flet ((do-it () ad-do-it))
    (when (string-equal (do-it) "")
      (setq ad-return-value
            ;; use \ below so this doesn't show up in searches
            "TOD\O"))))
(ad-activate 'grep-tag-default)

;; (define-key global-map (kbd "C-c C-v C-s") 'rgrep)

;;;; ___________________________________________________________________________

(provide 'nomis-searching)
