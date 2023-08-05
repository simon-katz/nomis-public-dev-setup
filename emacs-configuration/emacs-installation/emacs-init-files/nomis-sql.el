;;;; Init stuff -- Nomis SQL tailoring --  -*- lexical-binding: t -*-

;;;; ___________________________________________________________________________

(with-eval-after-load 'sql

  (defun sql-send-buffer ()
    "Send the buffer contents to the SQL process."
    (interactive)
    (message-box "sql-send-buffer is disabled — too easy to do it accidentally.")))

;;;; ___________________________________________________________________________

(provide 'nomis-sql)
