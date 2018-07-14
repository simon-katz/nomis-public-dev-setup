;;;; Init stuff -- Indenting s-expressions.

;;;; ___________________________________________________________________________
;;;; ---- Want indentation commands to work when at the end of a sexp ----

(defun just-after-close-paren-p ()
  (prog2
      (backward-char)
      (looking-at ")")
    (forward-char)))

(defun do-backward-sexp-before-indenting-p ()
  (and (just-after-close-paren-p)
       (not (looking-at "("))))

;;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(defun nomis/indent-pp-sexp ()
  (interactive)
  (save-excursion
    (cl-flet ((do-it () (indent-pp-sexp)))
      (if (not (do-backward-sexp-before-indenting-p))
          (do-it)
        (progn
          (backward-sexp)
          (do-it))))))

;;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(defun nomis/prog-indent-sexp ()
  (interactive)
  (save-excursion
    (cl-flet ((do-it () (prog-indent-sexp)))
      (if (not (do-backward-sexp-before-indenting-p))
          (do-it)
        (progn
          (backward-sexp)
          (do-it))))))

;;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(defun nomis/indent-sexp--top-level ()
  "Indent the enclosing top-level form using `prog-indent-sexp`."
  (interactive)
  (save-excursion
    (nomis-beginning-of-this-defun)
    (nomis/prog-indent-sexp)))

(defun nomis/indent-sexp--form-after-point ()
  "Indent the form after point using `prog-indent-sexp`."
  (interactive)
  (save-excursion
    (nomis/prog-indent-sexp)))

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

(define-key emacs-lisp-mode-map (kbd "C-M-q") 'nomis/indent-sexp--form-after-point)

(eval-after-load 'clojure-mode
  '(progn
     (define-key clojure-mode-map (kbd "C-M-q") 'nomis/indent-sexp--form-after-point)))

(eval-after-load 'paredit
  '(progn
     (define-key paredit-mode-map (kbd "M-q") 'nomis/indent-sexp--top-level)))

;;;; ___________________________________________________________________________

(provide 'nomis-indent-sexp)
