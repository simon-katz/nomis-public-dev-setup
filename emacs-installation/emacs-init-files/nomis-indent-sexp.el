;;;; Init stuff -- Indenting s-expressions.

;;;; ___________________________________________________________________________
;;;; ---- Want indentation commands to work when at the end of a sexp ----

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

(defun nomis-prog-indent-sexp--top-level ()
  "Indent the enclosing top-level form using `prog-indent-sexp`."
  (interactive)
  (save-excursion
    (prog-indent-sexp t)))

(defun nomis-prog-indent-sexp--form-after-point ()
  "Indent the form after point using `prog-indent-sexp`."
  (interactive)
  (save-excursion
    (prog-indent-sexp nil)))

;;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

;; The built-in key bindings are:
;; - C-M-q in elisp               indent-pp-sexp
;; - C-M-q in clojure             prog-indent-sexp
;; -   M-q in clojure and elisp   paredit-reindent-defun
;; 
;; Clojure indentation seems to work properly with `prog-indent-sexp`, but
;; not with the others.
;; 
;; WTF!
;; Let's rationalise:

(define-key emacs-lisp-mode-map (kbd "C-M-q") 'nomis-prog-indent-sexp--form-after-point)

(eval-after-load 'clojure-mode
  '(progn
     (define-key clojure-mode-map (kbd "C-M-q") 'nomis-prog-indent-sexp--form-after-point)))

(eval-after-load 'paredit
  '(progn
     (define-key paredit-mode-map (kbd "M-q") 'nomis-prog-indent-sexp--top-level)))

;;;; ___________________________________________________________________________

(provide 'nomis-indent-sexp)
