;;;; Init stuff -- clojure-mode extras

;;;; ___________________________________________________________________________
;;;; ---- Reader comments ----

;;;; Inspired by https://gist.github.com/4349847
;;;; ...which says...
;;;;     inspired by http://bc.tech.coop/blog/070122.html
;;;;     ported from slime/contrib/slime-editing-commands.el

(define-key clojure-mode-map (kbd "C-c ;")
  'nomis/clojure/insert-reader-comment)
(define-key clojure-mode-map (kbd "C-c :")
  'nomis/clojure/remove-reader-comment)

(defun nomis/clojure/insert-reader-comment (prefix)
  "Insert a reader comment (#_) around the s-expression containing the point.
If this command is invoked repeatedly (without any other command
occurring between invocations), the comment progressively moves outward
over enclosing expressions. If invoked with a positive prefix argument,
the s-expression prefix expressions out is enclosed in a set of balanced
comments."
  (interactive "*p")
  (save-excursion
    (if (eq last-command this-command)
        (when (or (looking-at-p "#_")
                  (search-backward "#_" nil t)) ; wrong -- not structure-aware
          (delete-char 2)
          (backward-up-list)
          (insert "#_"))
      (progn
        (nomis/move-to-start-of-bracketed-sexp-around-point)
        (dotimes (i (1- prefix))
          (backward-up-list)
          (decf prefix))
        (insert "#_")))))

(defun nomis/clojure/remove-reader-comment ()
  "Remove a reader comment enclosing point."
  (interactive "*")
  ;; wrong -- not structure-aware
  (save-excursion
    (forward-char 2)
    (when (search-backward "#_" nil t)
      (delete-char 2))))

;;;; ___________________________________________________________________________

(provide 'nomis-clojure-mode-extras)
