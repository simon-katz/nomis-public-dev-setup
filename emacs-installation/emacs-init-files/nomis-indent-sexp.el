;;;; Init stuff -- Indenting s-expressions.

;;;; TODO: Do you want this?
;;;;       Is the default horrible?
;;;;       Was there a Starter Kit issue that made you do this?

;;;; ___________________________________________________________________________
;;;; ---- Want C-M-q to work when at the end of a sexp ----

;; It's best to do this kind of thing (modifying an existing
;; command) using advice.
;; But don't delete the following because it has examples of modifying
;; modes that you may want to refer to.

;; (defun nomis-indent-sexp (arg)
;;   (interactive "P")
;;   (save-excursion
;;     (flet ((do-it () (indent-pp-sexp arg)))
;;       (if (or (looking-at "(")
;;               (let ((just-after-close-paren-p
;;                      (prog2
;;                          (backward-char)
;;                          (looking-at ")")
;;                        (forward-char))))
;;                 (not just-after-close-paren-p)))
;;           (do-it)
;;         (progn
;;           (backward-sexp)
;;           (do-it)
;;           ;; (forward-sexp) ; not needed because of save-excursion
;;           )))))

;;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(defadvice indent-pp-sexp (around work-when-at-end-of-sexp (&optional arg))
  (save-excursion
    (flet ((do-it () ad-do-it))
      (if (or (looking-at "(")
              (let ((just-after-close-paren-p
                     (prog2
                         (backward-char)
                         (looking-at ")")
                       (forward-char))))
                (not just-after-close-paren-p)))
          (do-it)
        (progn
          (backward-sexp)
          (do-it))))))

(ad-activate 'indent-pp-sexp)

;;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(defadvice prog-indent-sexp (around work-when-at-end-of-sexp (&optional arg))
  (save-excursion
    (flet ((do-it () ad-do-it))
      (if (or (looking-at "(")
              (let ((just-after-close-paren-p
                     (prog2
                         (backward-char)
                         (looking-at ")")
                       (forward-char))))
                (not just-after-close-paren-p)))
          (do-it)
        (progn
          (backward-sexp)
          (do-it))))))

(ad-activate 'prog-indent-sexp)

;;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(defun nomis-prog-indent-sexp (&optional arg)
  "Indent the enclosing top-level form.
When interactively called with prefix, indent the expression after point instead.
(The opposite of `prog-indent-sexp`.)"
  (interactive "P")
  (save-excursion
    (prog-indent-sexp (not arg))))

;;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

;; The built-in key bindings are:
;; - C-M-q in elisp               indent-pp-sexp
;; - C-M-q in clojure             prog-indent-sexp
;; -   M-q in clojure and elisp   paredit-reindent-defun

;; Clojure indentation seems to work properly with `prog-indent-sexp`, but
;; not with the others.

;; WTF!
;; Let's rationalise:

(define-key emacs-lisp-mode-map (kbd "C-M-q") 'nomis-prog-indent-sexp)

(eval-after-load 'clojure-mode
  '(progn
     (define-key clojure-mode-map (kbd "C-M-q") 'nomis-prog-indent-sexp)))

(eval-after-load 'paredit
  '(progn
     (define-key paredit-mode-map (kbd "M-q") 'nomis-prog-indent-sexp)))

;;;; ___________________________________________________________________________

(provide 'nomis-indent-sexp)
