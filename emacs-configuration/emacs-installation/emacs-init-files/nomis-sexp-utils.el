;;;; Init stuff -- SEXP utils

(defun nomis/looking-at-whitespace ()
  ;; (looking-at "[:space:]")
  ;; (looking-at "\\s-")
  ;; Neither of the above work, but IIUC they should.
  (looking-at "[ \t\n]"))

(defun nomis/looking-at-whitespace-start-p ()
  (and (nomis/looking-at-whitespace)
       (or (= (point) 1)
           (save-excursion
             (backward-char 1)
             (not (nomis/looking-at-whitespace))))))

(defun nomis/looking-between-forms ()
  (and (nomis/looking-at-whitespace)
       (not (nomis/looking-at-whitespace-start-p))))

(defvar *regexp-for-bracketed-sexp-start*
  "(\\|\\[\\|{\\|#{")

(defvar *regexp-for-bracketed-sexp-end*
  ")\\|]\\|}")

(defun nomis/looking-at-bracketed-sexp-start ()
  (looking-at *regexp-for-bracketed-sexp-start*))

(defun -nomis/looking-at-bracketed-sexp-end ()
  (looking-at *regexp-for-bracketed-sexp-end*))

(defun nomis/looking-after-bracketed-sexp-end ()
  (and (not (nomis/looking-at-bracketed-sexp-start))
       (and (not (= (point) 1))
            (save-excursion
              (backward-char 1)
              (-nomis/looking-at-bracketed-sexp-end)))))

(defun nomis/looking-at-end-of-empty-bracketed-sexp ()
  (and (-nomis/looking-at-bracketed-sexp-end)
       (and (not (= (point) 1))
            (save-excursion
              (backward-char 1)
              (nomis/looking-at-bracketed-sexp-start)))))

(defun nomis/looking-at-char-1-whitespace-p ()
  (and (= (point) 1)
       (nomis/looking-at-whitespace)))

(defun nomis/looking-at-multiple-whitespace ()
  (and (or (nomis/looking-at-whitespace)
           ;; FIXME What is this next bit for? When you have tests, try removing it.
           (= (point) (point-max)))
       (and (not (= (point) 1))
            (save-excursion
              (backward-char)
              (nomis/looking-at-whitespace)))))

(defun nomis/looking-after-bracketed-sexp-end-at-bracketed-sexp-end-or-whitespace ()
  (and (nomis/looking-after-bracketed-sexp-end)
       (or (nomis/looking-at-whitespace)
           (-nomis/looking-at-bracketed-sexp-end))))

(defun nomis/looking-at-whitespace-after-bracketed-sexp-start ()
  (and (nomis/looking-at-whitespace)
       (and (not (= (point) 1))
            (save-excursion
              (backward-char)
              (nomis/looking-at-bracketed-sexp-start)))))

(defun nomis/looking-at-regexp-before-bracketed-sexp-start (regexp)
  (looking-at (concatenate 'string
                           regexp
                           *regexp-for-bracketed-sexp-start*)))

(defun nomis/looking-at-boring-place-p ()
  ;; TODO: Add tests for these.
  ;;       But how?
  ;;       Can you set up a buffer, put text in it, set a position
  ;;       and then make it current?
  ;;       If you can, setting the position will be fiddly.
  ;;       Ah! Can have a function that processes a string and looks for ^ or ^^
  ;;       or something.
  (or (nomis/looking-at-bracketed-sexp-start)
      (nomis/looking-at-end-of-empty-bracketed-sexp)
      (nomis/looking-at-char-1-whitespace-p)
      (nomis/looking-at-multiple-whitespace)
      (nomis/looking-after-bracketed-sexp-end-at-bracketed-sexp-end-or-whitespace)
      (nomis/looking-at-whitespace-after-bracketed-sexp-start)
      (looking-at ";")
      (nomis/looking-at-regexp-before-bracketed-sexp-start "'")
      (nomis/looking-at-regexp-before-bracketed-sexp-start "`")
      (nomis/looking-at-regexp-before-bracketed-sexp-start "#'")))

(defun -nomis/forward-sexp-gives-no-error?
    ()
  (save-excursion
    (condition-case nil
        (progn (forward-sexp) t)
      (error nil))))

(defun -nomis/backward-sexp-gives-no-error?
    ()
  (save-excursion
    (condition-case nil
        (progn (backward-sexp) t)
      (error nil))))

(defun nomis/at-top-level? ()
  (save-excursion
    (condition-case nil
        (progn (paredit-backward-up) nil)
      (error t))))

(defun nomis/nesting-level ()
  (save-excursion
    (let* ((cnt 1))
      (while (not (nomis/at-top-level?))
        (incf cnt)
        (paredit-backward-up))
      cnt)))

(defun nomis/can-forward-sexp? ()
  ;; This is complicated, because `forward-sexp` behaves differently at end
  ;; of file and inside-and-at-end-of a `(...)` form.
  (cond ((not (nomis/at-top-level?))
         (-nomis/forward-sexp-gives-no-error?))
        ((and (thing-at-point 'symbol)
              (save-excursion (ignore-errors (forward-char) t))
              (save-excursion (forward-char) (thing-at-point 'symbol)))
         ;; We're on a top-level symbol (and not after its end).
         t)
        (t
         (or (bobp) ; should really check that there's an sexp ahead
             (condition-case nil
                 (not (= (save-excursion
                           (let* ((pos (point)))
                             (backward-sexp)
                             (point)))
                         (save-excursion
                           (let* ((pos (point)))
                             (forward-sexp)
                             (backward-sexp)
                             (point)))))
               (error nil))))))

(defun nomis/can-backward-sexp? ()
  ;; This is complicated, because `backward-sexp` behaves differently at end
  ;; of file and inside-and-at-beginning-of a `(...)` form.
  (cond ((not (nomis/at-top-level?))
         (-nomis/backward-sexp-gives-no-error?))
        ((and (thing-at-point 'symbol)
              (save-excursion (ignore-errors (backward-char) t))
              (save-excursion (backward-char) (thing-at-point 'symbol)))
         ;; We're on a top-level symbol, but not at its start
         t)
        (t
         (condition-case nil
             (not (= (save-excursion
                       (let* ((pos (point)))
                         (forward-sexp)
                         (point)))
                     (save-excursion
                       (let* ((pos (point)))
                         (backward-sexp)
                         (forward-sexp)
                         (point)))))
           (error nil)))))

(defun nomis/looking-at-beginning-of-sexp/kinda? ()
  "t if point can be arrived at with a `(backward-sexp)`, nil otherwise.

Note, for example, that for a quoted sexp, point would have to be
on the quote for this to return t."
  (and (nomis/can-forward-sexp?)
       (= (save-excursion (forward-sexp) (backward-sexp) (point))
          (point))))

(defun nomis/in-middle-of-symbol-ish? ()
  "t if point is in the middle of a symbol-and-any-prefix-characters,
nil otherwise.
Examples (| denotes cursor position):
    |xxxx -- nil
    x|xxx -- t
    xxx|x -- t
    xxxx| -- nil
    |'xxxx -- nil
    '|xxxx -- t"
  (and (nomis/can-forward-sexp?)
       (< (save-excursion (forward-sexp) (backward-sexp) (point))
          (point))))

(defun nomis/goto-beginning-of-sexp/or-end/forward ()
  "If point is at the beginning of an sexp, stay there.
Otherwise, if there is an sexp that begins after point and is at
the same level as point, go to its beginning.
Otherwise, if on a symbol, move to the end of the symbol.
Otherwise, stay at point."
  (when (nomis/in-middle-of-symbol-ish?)
    (forward-sexp))
  (when (nomis/can-forward-sexp?)
    (forward-sexp)
    (backward-sexp)))

(defun nomis/goto-beginning-of-sexp/or-end/backward ()
  "Like `nomis/goto-beginning-of-sexp/or-end` followed by `backward-sexp`.
This is the same as `backward-sexp`, but the intention is
slightly different.
Note that we can't end up at the end of an sexp unless we are
inside an empty form, in which case we get an error."
  (backward-sexp))

(defun nomis/next-sexp (&optional arg)
  "The inverse of `backward-sexp`, so you can step forward over forms with
point going to the beginning of each form.
If ARG is 1 or not supplied: go to the beginning of the next sexp.
If ARG is supplied, go to the beginning of the next-but-ARG-minus-1'th sexp."
  (interactive "^p")
  (let* ((hacked-arg-1 (or arg 1))
         (hacked-arg-2 (if (nomis/looking-at-beginning-of-sexp/kinda?)
                           hacked-arg-1
                         (1- hacked-arg-1))))
    (unless (zerop hacked-arg-2)
      ;; The above test is probably not necessary, but the behaviour of
      ;; `forward-sexp` is not documented when arg is zero, and it's better
      ;; to be safe.
      (forward-sexp hacked-arg-2))
    (if (-nomis/forward-sexp-gives-no-error?)
        (nomis/goto-beginning-of-sexp/or-end/forward)
      (forward-sexp) ; produce an error, for consistency with `forward-sexp`
      )))

(defun nomis/set-up-next-sexp-key ()
  (define-key paredit-mode-map (kbd "H-M-f") 'nomis/next-sexp)
  ;; (define-key paredit-mode-map (kbd "H-M-b") 'backward-sexp) ; key doesn't work on my Mac
  )

(add-hook 'paredit-mode-hook 'nomis/set-up-next-sexp-key)

(defun nomis/move-to-start-of-bracketed-sexp-around-point ()
  (cond ((nomis/looking-at-bracketed-sexp-start)
         ;; stay here
         )
        ((or (nomis/looking-at-whitespace)
             (nomis/looking-after-bracketed-sexp-end))
         (backward-sexp))
        (t
         (ignore-errors (forward-sexp))
         (backward-sexp))))

(defun nomis/beginning-of-top-level-form ()
  (nomis/end-of-top-level-form)
  (backward-sexp))

(defun nomis/end-of-top-level-form ()
  (when (nomis/looking-at-bracketed-sexp-start)
    (forward-sexp))
  (while (ignore-errors (paredit-backward-up -1) t))
  (when (or
         ;; between top-level forms
         (nomis/looking-between-forms)
         ;; in the middle of a top-level symbol
         (not (nomis/looking-at-whitespace)))
    (forward-sexp)))

(defun nomis/start-of-this-or-enclosing-form ()
  "If looking at beginning of bracketed sexp, stay there,
   else if looking after end of bracketed sexp, move to its start,
   else if within a bracketed sexp, move to its start,
   otherwise move to beginning of next top-level form."
  (cond ((nomis/looking-at-bracketed-sexp-start)
         ;; stay where we are
         )
        ((nomis/looking-after-bracketed-sexp-end)
         (ignore-errors (backward-sexp)))
        (t
         (unless (ignore-errors (paredit-backward-up)
                                t)
           (if (or (nomis/looking-at-whitespace-start-p)
                   (= (point) (point-max)))
               (backward-sexp)
             (progn
               (forward-sexp)
               (backward-sexp)))))))

(provide 'nomis-sexp-utils)
