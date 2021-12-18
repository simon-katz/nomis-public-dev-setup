;;;; Init stuff -- Clojure indentation.

;; (setq clojure-defun-style-default-indent t) ; TODO: Do you like this? No.

(eval-after-load 'clojure-mode
  '(define-clojure-indent
     ;; Ring and Compojure
     ;; From https://github.com/weavejester/compojure/wiki/Emacs-indentation.
     (defroutes 'defun)
     (GET 2)
     (POST 2)
     (PUT 2)
     (DELETE 2)
     (HEAD 2)
     (ANY 2)
     (context 2)
     ;; Midje
     (fact 'defun)
     (facts 'defun)
     (fact-group 'defun)
     (against-background 'defun)
     (provided 0)
     (for-all 2)
     ;; core.match
     (match 1)
     ;; Om & Fulcro
     (dom/a        1)
     (dom/button   1)
     (dom/div      1)
     (dom/form     1)
     (dom/h1       1)
     (dom/h2       1)
     (dom/h3       1)
     (dom/input    1)
     (dom/label    1)
     (dom/li       1)
     (dom/textarea 1)
     (dom/ul       1)))

;; (defvar nomis/clojure-indent-method-1-function-prefixes
;;   '("dom/"))

;; (defun nomis/clojure-indent-method-1-p (function-name)
;;   (loop for prefix in nomis/clojure-indent-method-1-function-prefixes
;;         for prefix-length = (length prefix)
;;         thereis (and (>= (length function-name)
;;                          prefix-length)
;;                      (equal (subseq function-name 0 prefix-length)
;;                             prefix))))

;; (defun nomis/clojure-indent-method (function)
;;   (let ((function-name (substring-no-properties function)))
;;     (or (get (intern-soft function-name) 'clojure-indent-function)
;;         (when (nomis/clojure-indent-method-1-p function-name)
;;           1))))

;; (cond
;;  ((member clojure-mode-version
;;           '("2.1.0"))
;;   (defun clojure-indent-function (indent-point state)
;;     "This function is the normal value of the variable `lisp-indent-function'.
;; It is used when indenting a line within a function call, to see if the
;; called function says anything special about how to indent the line.

;; INDENT-POINT is the position where the user typed TAB, or equivalent.
;; Point is located at the point to indent under (for default indentation);
;; STATE is the `parse-partial-sexp' state for that position.

;; If the current line is in a call to a Lisp function
;; which has a non-nil property `lisp-indent-function',
;; that specifies how to do the indentation.  The property value can be
;; * `defun', meaning indent `defun'-style;
;; * an integer N, meaning indent the first N arguments specially
;;   like ordinary function arguments and then indent any further
;;   arguments like a body;
;; * a function to call just as this function was called.
;;   If that function returns nil, that means it doesn't specify
;;   the indentation.

;; This function also returns nil meaning don't specify the indentation."
;;     (let ((normal-indent (current-column)))
;;       (goto-char (1+ (elt state 1)))
;;       (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t)
;;       (if (and (elt state 2)
;;                (not (looking-at "\\sw\\|\\s_")))
;;           ;; car of form doesn't seem to be a symbol
;;           (progn
;;             (if (not (> (save-excursion (forward-line 1) (point))
;;                         calculate-lisp-indent-last-sexp))
;;                 (progn (goto-char calculate-lisp-indent-last-sexp)
;;                        (beginning-of-line)
;;                        (parse-partial-sexp (point)
;;                                            calculate-lisp-indent-last-sexp 0 t)))
;;             ;; Indent under the list or under the first sexp on the same
;;             ;; line as calculate-lisp-indent-last-sexp.  Note that first
;;             ;; thing on that line has to be complete sexp since we are
;;             ;; inside the innermost containing sexp.
;;             (backward-prefix-chars)
;;             (if (and (eq (char-after (point)) ?\[)
;;                      (eq (char-after (elt state 1)) ?\())
;;                 (+ (current-column) 2) ;; this is probably inside a defn
;;               (current-column)))
;;         (let* ((function (buffer-substring (point)
;;                                            (progn (forward-sexp 1) (point))))
;;                (open-paren (elt state 1))
;;                (method nil)
;;                (function-tail (first
;;                                (last
;;                                 (split-string (substring-no-properties function) "/")))))
;;           (setq method (or (get (intern-soft function-tail) 'clojure-indent-function)
;;                            (nomis/clojure-indent-method function) ; jsk added this
;;                            ))
;;           (cond ((member (char-after open-paren) '(?\[ ?\{))
;;                  (goto-char open-paren)
;;                  (1+ (current-column)))
;;                 ((or (eq method 'defun)
;;                      (and clojure-defun-style-default-indent
;;                           ;; largely to preserve useful alignment of :require, etc in ns
;;                           (not (string-match "^:" function))
;;                           (not method))
;;                      (and (null method)
;;                           (> (length function) 3)
;;                           (string-match "\\`\\(?:\\S +/\\)?\\(def\\|with-\\)"
;;                                         function)))
;;                  (lisp-indent-defform state indent-point))

;;                 ((integerp method)
;;                  (lisp-indent-specform method state
;;                                        indent-point normal-indent))
;;                 (method
;;                  (funcall method indent-point state))
;;                 (clojure-use-backtracking-indent
;;                  (clojure-backtracking-indent
;;                   indent-point state normal-indent))))))))
;;  (t
;;   (message-box
;;    "You need to fix your clojure-indent-function for this version of clojure-mode.")))

;;;; ___________________________________________________________________________

(provide 'nomis-clojure-indentation)

