;;;; Init stuff -- SEXP utils

(defun nomis-looking-at-whitespace ()
  ;; (looking-at "[:space:]")
  ;; (looking-at "\\s-")
  ;; Neither of the above work, but IIUC they should.
  (looking-at "[ \t\n]"))

(defvar *regexp-for-bracketed-sexp-start*
  "(\\|\\[\\|{\\|#{")

(defvar *regexp-for-bracketed-sexp-end*
  ")\\|]\\|}")

(defun nomis-looking-at-bracketed-sexp-start ()
  (looking-at *regexp-for-bracketed-sexp-start*))

(defun nomis-looking-at-bracketed-sexp-end ()
  (looking-at *regexp-for-bracketed-sexp-end*))

(defun nomis-looking-after-bracketed-sexp-end ()
  (and (not (nomis-looking-at-bracketed-sexp-start))
       (and (not (= (point) 1))
            (save-excursion
              (backward-char 1)
              (nomis-looking-at-bracketed-sexp-end)))))

(defun nomis-looking-at-end-of-empty-bracketed-sexp ()
  (and (nomis-looking-at-bracketed-sexp-end)
       (and (not (= (point) 1))
            (save-excursion
              (backward-char 1)
              (nomis-looking-at-bracketed-sexp-start)))))

(defun nomis-looking-at-multiple-whitespace ()
  (and (or (nomis-looking-at-whitespace)
           (= (point) (point-max)))
       (and (not (= (point) 1))
            (save-excursion
              (backward-char)
              (nomis-looking-at-whitespace)))))

(defun nomis-looking-after-bracketed-sexp-end-at-bracketed-sexp-end-or-whitespace ()
  (and (nomis-looking-after-bracketed-sexp-end)
       (or (nomis-looking-at-whitespace)
           (nomis-looking-at-bracketed-sexp-end))))

(defun nomis-looking-at-whitespace-after-bracketed-sexp-start ()
  (and (nomis-looking-at-whitespace)
       (and (not (= (point) 1))
            (save-excursion
              (backward-char)
              (nomis-looking-at-bracketed-sexp-start)))))

(defun nomis-looking-at-regexp-before-bracketed-sexp-start (regexp)
  (looking-at (concatenate 'string
                           regexp
                           *regexp-for-bracketed-sexp-start*)))

(defun nomis-looking-at-boring-place-p ()
  ;; TODO: Add tests for these.
  ;;       But how?
  ;;       Can you set up a buffer, put text in it, set a position
  ;;       and then make it current?
  ;;       If you can, setting the position will be fiddly.
  ;;       Ah! Can have a function that processes a string and looks for ^ or ^^
  ;;       or something.
  (or (nomis-looking-at-bracketed-sexp-start)
      (nomis-looking-at-end-of-empty-bracketed-sexp)
      (nomis-looking-at-multiple-whitespace)
      (nomis-looking-after-bracketed-sexp-end-at-bracketed-sexp-end-or-whitespace)
      (nomis-looking-at-whitespace-after-bracketed-sexp-start)
      (looking-at ";")
      (nomis-looking-at-regexp-before-bracketed-sexp-start "'")
      (nomis-looking-at-regexp-before-bracketed-sexp-start "`")
      (nomis-looking-at-regexp-before-bracketed-sexp-start "#'")))

(defun nomis-move-to-start-of-bracketed-sexp-around-point ()
  (cond ((nomis-looking-at-bracketed-sexp-start)
         ;; stay here
         )
        ((or (nomis-looking-at-whitespace)
             (nomis-looking-after-bracketed-sexp-end))
         (backward-sexp 1))
        (t
         (ignore-errors (forward-sexp 1))
         (backward-sexp 1))))

(defun nomis-beginning-of-this-defun ()
  ;; this deals with most situations
  (while (ignore-errors (paredit-backward-up) t))
  ;; Check for Clojure #{
  (when (and (looking-at "{")
             (ignore-errors
               (save-excursion
                 (backward-char 1)
                 (looking-at "#"))))
    (backward-char 1))
  ;; this handles the case when we are between top-level forms
  (when (not (nomis-looking-at-bracketed-sexp-start))
    (backward-sexp)))

(defun nomis-end-of-this-defun ()
  (nomis-beginning-of-this-defun)
  (forward-sexp))

(provide 'nomis-sexp-utils)
