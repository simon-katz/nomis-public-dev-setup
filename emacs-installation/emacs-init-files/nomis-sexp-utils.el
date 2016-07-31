;;;; Init stuff -- SEXP utils

(defun nomis-looking-at-whitespace ()
  ;; (looking-at "[:space:]")
  ;; (looking-at "\\s-")
  ;; Neither of the above work, but IIUC they should.
  (looking-at "[ \t\n]"))

(defun nomis-looking-at-sexp-start ()
  (-some-p #'looking-at '("(" "\\[" "{" "#{")))

(defun nomis-looking-at-sexp-end ()
  (-some-p #'looking-at '(")" "]" "}")))

(defun nomis-looking-after-sexp-end ()
  (and (not (nomis-looking-at-sexp-start))
       (and (not (= (point) 1))
            (save-excursion
              (backward-char 1)
              (nomis-looking-at-sexp-end)))))

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

(defvar nomis-looking-at-interesting-place-boring-chars
  '(?\; ?' ?`))

(defun nomis-looking-at-interesting-place-p ()
  (and (not (nomis-looking-at-sexp-start))
       (not (nomis-looking-at-multiple-whitespace))
       (not (nomis-looking-after-sexp-end-at-sexp-end-or-whitespace))
       (not (member (char-after)
                    nomis-looking-at-interesting-place-boring-chars))))

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
