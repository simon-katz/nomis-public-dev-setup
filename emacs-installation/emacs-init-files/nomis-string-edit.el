;;;; Init stuff -- String editing --  -*- lexical-binding: t -*-

(require 'dash)
(require 's)
(require 'cl)

;;;; ___________________________________________________________________________

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
  (atomic-change-group
    (let* ((start-line (line-number-at-pos))
           (start-col (current-column))
           (n-spaces-at-start-of-start-line (save-excursion
                                              (beginning-of-line)
                                              (let* ((cnt 0))
                                                (while (looking-at-p " ")
                                                  (incf cnt)
                                                  (forward-char))
                                                cnt))))
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
               (out-string (-nomis/indent-string* in-string indent)))
          (unless (equal in-string out-string)
            ;; Insert and delete a char at start point so that undo will take the
            ;; cursor there.
            (insert "x")
            (delete-char -1)
            (goto-char string-start-point)
            (delete-region (point)
                           (save-excursion (forward-sexp 1) (point)))
            (insert out-string)
            (goto-line start-line)
            (move-to-column new-col)))))))

;;;; ___________________________________________________________________________

(provide 'nomis-string-edit)
