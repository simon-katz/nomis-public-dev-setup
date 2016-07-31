;;;; Init stuff -- SEXP utils

(defun nomis-looking-at-whitespace ()
  ;; (looking-at "[:space:]")
  ;; (looking-at "\\s-")
  ;; Neither of the above work, but IIUC they should.
  (looking-at "[ \t\n]"))

(defvar regexp-for-sexp-start
  "(\\|\\[\\|{\\|#{")

(defvar regexp-for-sexp-end
  ")\\|]\\|}")

(defun nomis-looking-at-sexp-start ()
  (looking-at regexp-for-sexp-start))

(defun nomis-looking-at-sexp-end ()
  (looking-at regexp-for-sexp-end))

(defun nomis-looking-after-sexp-end ()
  (and (not (nomis-looking-at-sexp-start))
       (and (not (= (point) 1))
            (save-excursion
              (backward-char 1)
              (nomis-looking-at-sexp-end)))))

(defun nomis-looking-at-end-of-empty-sexp ()
  (and (nomis-looking-at-sexp-end)
       (and (not (= (point) 1))
            (save-excursion
              (backward-char 1)
              (nomis-looking-at-sexp-start)))))

(defun nomis-looking-at-multiple-whitespace ()
  (and (or (nomis-looking-at-whitespace)
           (= (point) (point-max)))
       (and (not (= (point) 1))
            (save-excursion
              (backward-char)
              (nomis-looking-at-whitespace)))))

(defun nomis-looking-after-sexp-end-at-sexp-end-or-whitespace ()
  (and (nomis-looking-after-sexp-end)
       (or (nomis-looking-at-whitespace)
           (nomis-looking-at-sexp-end))))

(defun nomis-looking-at-regexp-before-sexp-start (regexp)
  (looking-at (concatenate 'string
                           regexp
                           regexp-for-sexp-start)))

(defun nomis-looking-at-interesting-place-p ()
  (and (not (nomis-looking-at-sexp-start))
       (not (nomis-looking-at-end-of-empty-sexp))
       (not (nomis-looking-at-multiple-whitespace))
       (not (nomis-looking-after-sexp-end-at-sexp-end-or-whitespace))
       (not (looking-at ";"))
       (not (nomis-looking-at-regexp-before-sexp-start "'"))
       (not (nomis-looking-at-regexp-before-sexp-start "`"))
       (not (nomis-looking-at-regexp-before-sexp-start "#'"))))

(defun nomis-move-to-start-of-sexp-around-point ()
  (cond ((nomis-looking-at-sexp-start)
         ;; stay here
         )
        ((or (nomis-looking-at-whitespace)
             (nomis-looking-after-sexp-end))
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
  (when (not (nomis-looking-at-sexp-start))
    (backward-sexp)))

(provide 'nomis-sexp-utils)
