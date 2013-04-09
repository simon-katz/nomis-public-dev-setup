;;;; Init stuff -- Searching.

;;;; ___________________________________________________________________________
;;;; ---- Generic grep stuff ----

(progn
  ;; M-> and M-< are by default bound to end-of-buffer and
  ;; beginning-of-buffer, which are also bound to C-end and C-home.
  ;; Rebind M-> and M-< without much lossage.
  (define-key global-map (kbd "M->") 'next-error)
  (define-key global-map (kbd "M-<") 'previous-error))

;;;; ___________________________________________________________________________
;;;; ---- Stuff for rgrep and lgrep ----

(defadvice grep-tag-default (around change-space-to-todo ())
  (flet ((do-it () ad-do-it))
    (when (string-equal (do-it) "")
      (setq ad-return-value
            ;; use \ below so this doesn't show up in searches
            "TOD\O"))))
(ad-activate 'grep-tag-default)

;; (define-key global-map (kbd "C-c C-v C-s") 'rgrep)
