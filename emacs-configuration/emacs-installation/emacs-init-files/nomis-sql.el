;;;; Init stuff -- Nomis SQL tailoring --  -*- lexical-binding: t -*-

;;;; ___________________________________________________________________________

(with-eval-after-load 'sql

  (defun sql-send-buffer ()
    "Send the buffer contents to the SQL process."
    (interactive)
    (message-box "sql-send-buffer is disabled — too easy to do it accidentally."))

  (require 'eval-sexp-fu)
  (define-eval-sexp-fu-flash-command sql-send-paragraph
    (eval-sexp-fu-flash (cons (save-excursion
                                (backward-paragraph)
                                (point))
                              (save-excursion
                                (forward-paragraph)
                                (point)))))

  ;; I'm used to this key binding in TablePlus:
  (define-key sql-mode-map (kbd "M-RET") 'sql-send-paragraph))

;;;; ___________________________________________________________________________

(provide 'nomis-sql)
