;;;; nomis-logview.el --- logview tailoring ---  -*- lexical-binding: t -*-

(require 'logview)

;;;; ___________________________________________________________________________

(setq logview-auto-revert-mode 'auto-revert-tail-mode)

(setq logview-show-ellipses nil)

(pushnew '("nomis-extra: ISO 8601 datetime + millis using comma"
           (java-pattern . "yyyy-MM-dd HH:mm:ss,SSS"))
         logview-additional-timestamp-formats
         :test 'equal)

;;;; ___________________________________________________________________________

(provide 'nomis-logview)
