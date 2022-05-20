;;;; Init stuff -- String editing --  -*- lexical-binding: t -*-

(require 'dash)
(require 's)
(require 'cl)
(require 'nomis-msg)

;;;; ___________________________________________________________________________
;;;; ---- nomis/indent-string ----

(defun -nomis/indent-string* (s indent)
  (let* ((prefix-string (make-string indent ?\s))
         (lines (->> s
                     (s-split "\n")
                     (-map #'s-trim)))
         (first-line (first lines))
         (rest-lines (rest lines)))
    (s-join "\n"
            (cons first-line
                  (-map (lambda (s)
                          (if (s-blank? s) "" (s-concat prefix-string s)))
                        rest-lines)))))

(defun nomis/indent-string (prefix)
  "Indent the string that contains point so that all lines are
indented by the same amount, aligned under the first char after
the opening quote. Also remove whitespace at the end of lines.
With a prefix argument, align under the quote at the start of the
string."
  (interactive "*P")
  (let* ((start-line (line-number-at-pos))
         (start-col (current-column))
         (n-spaces-at-start-of-start-line (save-excursion
                                            (beginning-of-line)
                                            (let* ((cnt 0))
                                              (while (looking-at-p " ")
                                                (cl-incf cnt)
                                                (forward-char))
                                              cnt))))
    (atomic-change-group
      (cl-multiple-value-bind (string-start-point
                               string-start-col
                               string-start-line
                               in-string)
          (save-excursion
            (while (not (or (= (point) 1)
                            (looking-at-p "\"")))
              (backward-char))
            (when (not (looking-at-p "\""))
              (error "Not in a string"))
            (list (point)
                  (current-column)
                  (line-number-at-pos)
                  (nomis/grab-text)))
        (let* ((indent (+ string-start-col (if prefix 0 1)))
               (new-col (if (= start-line string-start-line)
                            start-col
                          (+ start-col
                             indent
                             (- n-spaces-at-start-of-start-line))))
               (new-string (-nomis/indent-string* in-string indent)))
          (if (equal in-string new-string)
              (progn
                (message "No changes")
                (beep))
            (progn
              ;; Insert and delete a char at start point so that undo will take
              ;; the cursor there.
              (insert "x")
              (delete-char -1)
              (goto-char string-start-point)
              (delete-region (point)
                             (save-excursion (forward-sexp 1) (point)))
              (insert new-string)
              (goto-line start-line)
              (move-to-column new-col))))))))

(define-key global-map (kbd "C-c C-g")
  'nomis/indent-string)

;;;; ___________________________________________________________________________
;;;; ---- nomis/chop-seq ----
;;;; ---- nomis/rearrange-string-into-lines ----
;;;; ---- nomis/rearrange-string ----

;;;; Some of this is a translation of Clojure code in
;;;; `com.nomistech.emacs-hacks-in-clojure`.

(defun -nomis/add-char (sofar c separator max-line-length)
  (let ((lines-sofar           (gethash :lines-sofar sofar))
        (line-sofar            (gethash :line-sofar sofar))
        (word-sofar            (gethash :word-sofar sofar))
        (line-sofar-choppable? (gethash :line-sofar-choppable? sofar)))
    ;; We use the terms "lines", "line" and "word", but we are dealing with
    ;; lists. These are misnomers, but easy to understand. Imagine we are
    ;; dealing with strings and characters.
    (if (eql separator c)
        (cond
         ;; Special case: if c is a leading separator, add it to `word-sofar`.
         ((and (zerop (length lines-sofar))
               (null line-sofar)
               (--every? (eql it separator) word-sofar))
          (puthash :word-sofar (cons c word-sofar) sofar)
          sofar)
         ;; If we have a word of length `max-line-length` or more, add it to
         ;; `lines-sofar`. (It will be at the beginning of a line, because we
         ;; will have started a new line when we previously reached
         ;; `max-line-length`.)
         ((>= (length word-sofar)
              max-line-length)
          (cl-assert (null line-sofar))
          (puthash :lines-sofar (cons word-sofar lines-sofar) sofar)
          (puthash :line-sofar '() sofar)
          (puthash :word-sofar '() sofar)
          (puthash :line-sofar-choppable? nil sofar)
          sofar)
         ;; Add word to `line-sofar`.
         (t
          (let ((line-sofar-new (-concat word-sofar
                                         (when line-sofar (list separator))
                                         line-sofar)))
            (puthash :line-sofar line-sofar-new sofar)
            (puthash :word-sofar '() sofar)
            (puthash :line-sofar-choppable? t sofar)
            sofar)))
      (if (or (not line-sofar-choppable?)
              (< (+ 1 (length line-sofar) (length word-sofar))
                 max-line-length))
          ;; `line-sofar` is not choppable or we can add to `word-sofar` without
          ;; exceeding `max-line-length`, so add c to `word-sofar`.
          (progn
            (puthash :word-sofar (cons c word-sofar) sofar)
            sofar)
        ;; We need a new line.
        (progn
          (puthash :lines-sofar (cons line-sofar lines-sofar) sofar)
          (puthash :line-sofar '() sofar)
          (puthash :word-sofar (cons c word-sofar) sofar)
          (puthash :line-sofar-choppable? nil sofar)
          sofar)))))

(defun nomis/chop-seq (separator s max-piece-length)
  (let* ((reduce-result (reduce (lambda (sofar c)
                                  (-nomis/add-char sofar
                                                   c
                                                   separator
                                                   max-piece-length))
                                s
                                :initial-value
                                (make-hash-table ;; :lines-sofar '()
                                 ;; :line-sofar  '()
                                 ;; :word-sofar  '()
                                 ;; :line-sofar-choppable? nil
                                 )))
         (lines-sofar (gethash :lines-sofar reduce-result))
         (line-sofar  (gethash :line-sofar reduce-result))
         (word-sofar  (gethash :word-sofar reduce-result)))
    (reverse
     (-map #'reverse
           (cons (if (null line-sofar)
                     word-sofar
                   (-concat word-sofar (cons separator line-sofar)))
                 lines-sofar)))))

(defun nomis/rearrange-string-into-lines (string left-margin right-margin)
  (let* ((single-line              (replace-regexp-in-string "[\n ]+"
                                                             " "
                                                             string
                                                             t
                                                             t))
         (max-line-length          (- right-margin left-margin))
         (chopped-seq              (nomis/chop-seq ?\s
                                                   single-line
                                                   max-line-length))
         (n-chopped-seqs           (length chopped-seq))
         (newline-and-left-padding (-concat (list ?\n)
                                            (-repeat left-margin ?\s)))
         (result-as-seqs           (-interleave
                                    chopped-seq
                                    (-concat (-repeat (1- n-chopped-seqs)
                                                      newline-and-left-padding)
                                             '(())))))
    (apply #'string
           (apply #'-concat result-as-seqs))))

;;## ;;;; ****
;;## ;;;; + Ensure `nomis/grab-text' has no free variables.
;;## ;;;;
;;## ;;;; + Put all your code-manipulation Clojure functions in single file in
;;## ;;;;   a new project.
;;## ;;;;   And have proper tests of the code-manipulation code.
;;## ;;;;

;;## (defun get-string-from-file (filePath)
;;##   "Return FILEPATH's file content."
;;##   ;; http://xahlee.blogspot.co.uk/2010/09/elisp-read-file-content-in-one-shot.html
;;##   ;; which says:
;;##   ;;   thanks to “Pascal J Bourguignon”
;;##   ;;   and "TheFlyingDutchman <zzbba...@aol.com>". 2010-09-02
;;##   ;;
;;##   ;; I changed insert-file-contents to insert-file-contents-literally
;;##   (with-temp-buffer
;;##     (insert-file-contents-literally filePath)
;;##     (buffer-string)))

(defun nomis/rearrange-string (prefix)
  "Rearrange string into lines. Without a prefix argument, indent
second and subsequent lines so that they line up sensibly with
the first line. With a prefix argument, indent second and
subsequent lines one character less as is the convention for
Clojure doc strings (which is stupid)."
  (interactive "*P")
  (if nil ; (y-or-n-p "Use `fill-paragraph` instead?")
      (fill-paragraph)
    (atomic-change-group
      (cl-multiple-value-bind (string-start-point
                               string-start-col
                               string-start-line
                               in-string)
          (save-excursion
            (while (not (or (= (point) 1)
                            (looking-at-p "\"")))
              (backward-char))
            (when (not (looking-at-p "\""))
              (error "Not in a string"))
            (list (point)
                  (current-column)
                  (line-number-at-pos)
                  (nomis/grab-text)))
        (let* ((new-string (nomis/rearrange-string-into-lines
                            in-string
                            (+ string-start-col
                               (if prefix 0 1))
                            72)))
          (if (equal in-string new-string)
              (progn
                (message "No changes")
                (beep))
            (progn
              ;; Insert and delete a char at start point so that undo will take
              ;; the cursor there.
              (insert "x")
              (delete-char -1)
              (goto-char string-start-point)
              (delete-region (point)
                             (save-excursion (forward-sexp 1) (point)))
              (insert new-string)
              (goto-char string-start-point))))))))

(define-key global-map (kbd "C-S-c C-S-g")
  'nomis/rearrange-string)

;;;; ___________________________________________________________________________

(provide 'nomis-string-edit)
