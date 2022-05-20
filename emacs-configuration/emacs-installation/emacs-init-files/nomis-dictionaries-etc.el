;;;; nomis-dictionaries-etc.el --- Dictionaries etc ---  -*- lexical-binding: t -*-

;;;; ___________________________________________________________________________
;;;; ---- lookup-word-at-point ----

(defun nomis/lookup-word-at-point ()
  "Lookup word at point in dictionary."
  (interactive)
  (cl-case system-type
    (darwin
     (call-process-shell-command
      (format "open dict:///%s/" (word-at-point))))
    (t
     (error "Need to implement `lookup-word-at-point`"))))

(provide 'nomis-dictionaries-etc)
