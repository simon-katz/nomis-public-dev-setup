;;; nomis-very-large-files.el -*- lexical-binding:t

;;;; ___________________________________________________________________________

(require 'nomis-find-file-noselect-without-confusing-messages)

(setq large-file-warning-threshold (ceiling (* 1024 1024))) ; 1 MB

;;;; ___________________________________________________________________________

(provide 'nomis-very-large-files)