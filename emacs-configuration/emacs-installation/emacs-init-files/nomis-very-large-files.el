;;; nomis-very-large-files.el -*- lexical-binding:t

;;;; ___________________________________________________________________________

(require 'nomis-find-file-noselect-without-confusing-messages)

;;;; ___________________________________________________________________________
;;;; Don't freeze when showing very large files

;;;; We're keeping some history and notes here -- might be useful in the future.

;;;; -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  - -
;;;; Attempt #1

;;;; See https://stackoverflow.com/a/18317181

;; (defun nomis/find-file-check-make-large-file-read-only-hook ()
;;   "If a file is over a given size, disable lots of things to prevent freezes."
;;   (let* ((size (buffer-size)))
;;     (when (> size (* 1024 1024))
;;       (case 2
;;         (1 (setq buffer-read-only t)
;;            (buffer-disable-undo)
;;            (fundamental-mode))
;;         (2 (kill-buffer (current-buffer))
;;            (let ((msg (format "That file is very large (about %.1f MB); not opening it in case Emacs hangs"
;;                               (/ (float size) (* 1024 1024)))))
;;              (message-box "%s." msg)
;;              (error "%s" msg )))))))

;; (add-hook 'find-file-hook 'nomis/find-file-check-make-large-file-read-only-hook)

;;;; -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  - -
;;;; Attempt #2

(setq large-file-warning-threshold (ceiling (* 1024 1024))) ; 1 MB

;;;; -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  - -
;;;; Attempt #3

;;;; Ah! There's a problem with `diff-hl-mode`. The freeze happened consistently
;;;; when adding a large number of lines and then attempting an undo.
;;;;
;;;; Now we're turning `diff-hl-mode` off for large files.

;;;; ___________________________________________________________________________

(provide 'nomis-very-large-files)
