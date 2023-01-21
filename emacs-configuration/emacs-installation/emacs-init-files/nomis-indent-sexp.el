;;;; Init stuff -- Indenting s-expressions.

;;;; ___________________________________________________________________________

(require 'eval-sexp-fu)
(require 'nomis-sexp-utils)

;;;; ___________________________________________________________________________
;;;; ---- Don't indent ";;" comments at bracketed-sexp-start ----

(defvar nomis/no-space-before-two-semicolon-comment-after-sexp-start? t)

(advice-add
 'comment-choose-indent
 :around
 (lambda (orig-fun &rest args)
   (or (and nomis/no-space-before-two-semicolon-comment-after-sexp-start?
            (not (bolp))
            (looking-at ";;")
            ;; Go to first non-whitespace before this comment and check whether
            ;; it is a bracketed-sexp-start. If it is, return the column
            ;; following the bracketed-sexp-start.
            (save-excursion (while (progn
                                     (backward-char)
                                     (and (not (bolp))
                                          (nomis/looking-at-whitespace))))
                            (when (nomis/looking-at-bracketed-sexp-start)
                              (1+ (current-column)))))
       (apply orig-fun args)))
 '((name . nomis/-no-space-before-two-semicolon-comment-after-sexp-start)))

(when nil ; Code to remove advice when in dev.
  (advice-remove 'comment-choose-indent
                 'nomis/-no-space-before-two-semicolon-comment-after-sexp-start)
  )

;;;; ___________________________________________________________________________
;;;; ---- Support for flashing forms when indenting ----

(defvar nomis/prog-indent-sexp-flash-duration 0.2)
(defvar nomis/prog-indent-sexp-flash-colour "PeachPuff1")

(defface nomis/prog-indent-sexp-flash-face
  ;; Copied from `eval-sexp-fu-flash`, and changed.
  `((((class color)) ,(list :background nomis/prog-indent-sexp-flash-colour
                            :foreground "black"
                            :bold nil))
    (t (:inverse-video t)))
  "Face for highlighting sexps during indentation."
  :group 'eval-sexp-fu ; is this right?
  )

(defmacro nomis/define-indent-command (indentation-command
                                       &rest move-and-call-advice-commands)
  "For `indentation-command`, add advice to do the following:
   - Position the cursor at the start of the targetted form.
   - Flash the targetted form.
   Within `move-and-call-advice-commands`, you should move the cursor to the
   start of the targetted form and call `(%do-indentation%)` to run the
   underlying command."
  (declare (indent 1))
  `(progn
     (define-eval-sexp-fu-flash-command ,indentation-command
       (eval-sexp-fu-flash (cons (point)
                                 (save-excursion
                                   (forward-sexp)
                                   (point)))))
     (advice-add ',indentation-command
                 :around
                 ,(cl-case 2 ; an easy way to turn off the advice if you feel a need to
                    (1 `(lambda (orig-fun &rest args)
                          (apply orig-fun args)))
                    (2 `(lambda (orig-fun &rest args)
                          (let* ((eval-sexp-fu-flash-duration
                                  nomis/prog-indent-sexp-flash-duration)
                                 (eval-sexp-fu-flash-face
                                  'nomis/prog-indent-sexp-flash-face))
                            (cl-labels ((%do-indentation%
                                         ()
                                         (apply orig-fun args)))
                              (save-excursion
                                ,@move-and-call-advice-commands))))))
                 '((name . nomis/with-flash-for-indenting)))))

;;;; ___________________________________________________________________________
;;;; ---- Flash forms when indenting ----

;;;; The built-in key bindings are:
;;;; - C-M-q in elisp               indent-pp-sexp
;;;; - C-M-q in clojure             prog-indent-sexp
;;;; -   M-q in clojure and elisp   paredit-reindent-defun

(progn ; Override keyboard bindings
  (define-key emacs-lisp-mode-map (kbd "C-M-q") 'nomis/indent-pp-sexp)
  (eval-after-load 'clojure-mode
    '(define-key clojure-mode-map (kbd "C-M-q") 'nomis/prog-indent-sexp))
  (eval-after-load 'paredit
    '(define-key paredit-mode-map (kbd "M-q") 'nomis/paredit-reindent-defun)))

(defun nomis/indent-pp-sexp (&optional arg)
  (interactive "P")
  (indent-pp-sexp))

(defun nomis/prog-indent-sexp (&optional arg)
  (interactive "P")
  (prog-indent-sexp))

(defun nomis/paredit-reindent-defun (&optional arg)
  (interactive "P")
  (paredit-reindent-defun))

(nomis/define-indent-command nomis/indent-pp-sexp
  (nomis/start-of-this-or-enclosing-form)
  (%do-indentation%))

(nomis/define-indent-command nomis/prog-indent-sexp
  (nomis/start-of-this-or-enclosing-form)
  (%do-indentation%))

(nomis/define-indent-command nomis/paredit-reindent-defun
  (nomis/beginning-of-top-level-form)
  (let* ((in-a-top-level-symbol-p (not
                                   (nomis/looking-at-bracketed-sexp-start))))
    (cond (in-a-top-level-symbol-p
           ;; We are on a top-level symbol. `paredit-reindent-defun` would
           ;; re-indent a nearby non-symbol top-level form; instead of that
           ;; don't do anything.
           (nomis/msg/beep))
          ((or (bound-and-true-p cider-mode)
               (eql major-mode 'cider-repl-mode))
           ;; `paredit-reindent-defun` in Clojure doesn't indent properly.
           ;; I think it does the cljfmt thing rather than the CIDER thing.
           ;; Instead, do something that gives proper indentation.
           (nomis/prog-indent-sexp))
          (t
           (%do-indentation%)))))

;;;; ___________________________________________________________________________

(provide 'nomis-indent-sexp)
