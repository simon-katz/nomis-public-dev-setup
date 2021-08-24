;;;; 'nomis-datetime.el --- datetime tailoring ---  -*- lexical-binding: t -*-

;;;; ___________________________________________________________________________

(setq datetime-timezone
      ;; Suddenly I began getting
      ;;   "Failed to determine system timezone; consider customizing
      ;;    ‘datetime-timezone’ variable"
      ;; warnings in .log files, and they stopped auto-tailing.
      ;; This fixes that.
      'Europe/London)

;;;; ___________________________________________________________________________

(provide 'nomis-datetime)
