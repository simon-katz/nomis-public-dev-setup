;;;; nomis-logview.el --- logview tailoring ---  -*- lexical-binding: t -*-

(require 'logview)

;;;; ___________________________________________________________________________

(setq logview-auto-revert-mode 'auto-revert-tail-mode)

(setq logview-show-ellipses nil)

;;;; ___________________________________________________________________________

(provide 'nomis-logview)
