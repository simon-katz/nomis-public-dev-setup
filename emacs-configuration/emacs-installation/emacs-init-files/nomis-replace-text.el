;;;; Init stuff -- nomis-replace-text --  -*- lexical-binding: t -*-

(defconst -nomis/replace-text-text-pairs
  '(;; Electric Clojure client-only
    ("#_{:clj-kondo/ignore #?(:clj [:unresolved-symbol] :cljs [])}"
     "•")
    ("#_{:clj-kondo/ignore #?(:clj [:unresolved-var] :cljs [])}"
     "•")
    ("#_{:clj-kondo/ignore #?(:clj [:unresolved-namespace] :cljs [])}"
     "•")
    ("#_{:clj-kondo/ignore #?(:clj true :default nil)}"
     "◆")
    ;; Electric Clojure server-only
    ("#_{:clj-kondo/ignore #?(:clj [] :cljs [:unresolved-symbol])}"
     "●")
    ("#_{:clj-kondo/ignore #?(:clj [] :cljs [:unresolved-var])}"
     "●")
    ("#_{:clj-kondo/ignore #?(:clj [] :cljs [:unresolved-namespace])}"
     "●")
    ("#_{:clj-kondo/ignore #?(:clj nil :default true)}"
     "■")))

(defconst -nomis/replace-text-keywords
  (cl-loop for (old new eat-whitespace?) in -nomis/replace-text-text-pairs
           collect `(,(concat "\\(" (regexp-quote old) "\\)")
                     (0 (progn (compose-region (match-beginning 1)
                                               (match-end 1)
                                               ,new)
                               (put-text-property (match-beginning 1)
                                                  (match-end 1)
                                                  'face
                                                  font-lock-comment-face)
                               nil)))))

(defun -nomis/replace-text-remove-stuff ()
  (save-excursion
    (cl-loop for (old new) in -nomis/replace-text-text-pairs
             do (progn
                  (goto-char (point-min))
                  (while (re-search-forward (regexp-quote old) nil t)
                    (decompose-region (match-beginning 0) (match-end 0)))))))

(define-minor-mode nomis/replace-text-mode
  "Replace some text with less text"
  nil nil nil
  (if nomis/replace-text-mode
      (progn
        (font-lock-add-keywords nil -nomis/replace-text-keywords)
        (org-bullets--fontify-buffer))
    (progn (font-lock-remove-keywords nil -nomis/replace-text-keywords)
           (-nomis/replace-text-remove-stuff)
           (org-bullets--fontify-buffer))))

;;;; ___________________________________________________________________________

(provide 'nomis-replace-text)
