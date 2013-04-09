;;;; Init stuff -- Indenting s-expressions.


;;;; ___________________________________________________________________________
;;;; ---- Want C-M-q to work when at the end of a sexp ----

;; It's best to do this kind of thing (modifying an existing
;; command) using advice.
;; But don't delete the following because it has examples of modifying
;; modes that you may want to refer to.

;; (defun nomis-indent-sexp (arg)
;;   (interactive "P")
;;   (save-excursion
;;     (flet ((do-it () (indent-pp-sexp arg)))
;;       (if (or (looking-at "(")
;;               (let ((just-after-close-paren-p
;;                      (prog2
;;                          (backward-char)
;;                          (looking-at ")")
;;                        (forward-char))))
;;                 (not just-after-close-paren-p)))
;;           (do-it)
;;         (progn
;;           (backward-sexp)
;;           (do-it)
;;           ;; (forward-sexp) ; not needed because of save-excursion
;;           )))))

(defadvice indent-pp-sexp (around work-when-at-end-of-sexp (&optional arg))
  (save-excursion
    (flet ((do-it () ad-do-it))
      (if (or (looking-at "(")
              (let ((just-after-close-paren-p
                     (prog2
                         (backward-char)
                         (looking-at ")")
                       (forward-char))))
                (not just-after-close-paren-p)))
          (do-it)
        (progn
          (backward-sexp)
          (do-it))))))

(ad-activate 'indent-pp-sexp)

(define-key emacs-lisp-mode-map (kbd "C-M-q") 'indent-pp-sexp)
(eval-after-load 'clojure-mode
  '(define-key clojure-mode-map (kbd "C-M-q") 'indent-pp-sexp))

