;;;; Init stuff -- clojure-mode extras

;;;; ___________________________________________________________________________
;;;; ---- Reader comments ----

;;;; Inspired by https://gist.github.com/4349847
;;;; ...which says...
;;;;     inspired by http://bc.tech.coop/blog/070122.html
;;;;     ported from slime/contrib/slime-editing-commands.el

(define-key clojure-mode-map (kbd "H-;")
  'nomis/clojure/insert-reader-comment)

(define-key clojure-mode-map (kbd "H-M-;")
  'nomis/clojure/move-reader-comment-up)

(define-key clojure-mode-map (kbd "H-:")
  'nomis/clojure/remove-reader-comment)

(defun nomis/clojure/move-reader-comment-up ()
  "Move the current or previous reader comment up a level."
  (interactive)
  (unless (member last-command
                  '(nomis/clojure/move-reader-comment-up
                    nomis/clojure/insert-reader-comment))
    ;; Because otherwise it's too easy to accidentally change code that's not
    ;; visible in the current window. The use of `search-backward` is bad, and
    ;; can end up making changes miles away.
    (error "Can only move reader comment up immediately after it's been inserted."))
  (save-excursion
    (forward-char 2)
    (if (not (search-backward "#_" nil t)) ; wrong -- not structure-aware
        (error "No reader comment found")
      (progn
        (when (nomis/sexp-at-top-level?)
          (error "Reader comment is already at top level"))
        (while (looking-at-p "#_") (forward-char 2))
        (backward-char 2)
        (let ((cnt 0))
          (while (looking-at-p "#_") (cl-incf cnt) (backward-char 2))
          (forward-char 2)
          (dotimes (_ cnt) (delete-char 2))
          (backward-up-list)
          (dotimes (_ cnt) (insert "#_"))
          (dotimes (_ cnt) (backward-char 2)))))))

(defun nomis/clojure/insert-reader-comment (n)
  "Insert a reader comment (#_) around the s-expression containing the point.
With a zero prefix argument, move the current or previous reader
comment up a level. With a positive prefix argument, insert that
number of reader comments."
  (interactive "*p")
  (save-excursion
    (if (zerop n) ; (eq last-command this-command)
        (nomis/clojure/move-reader-comment-up)
      (progn
        (nomis/move-to-start-of-bracketed-sexp-around-point)
        (dotimes (_ n)
          (insert "#_"))))))

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
