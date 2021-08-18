;;;; 'nomis-disable-c-z.el --- datetime tailoring ---  -*- lexical-binding: t -*-

;;;; ___________________________________________________________________________
;;;; ---- Unset C-z ----
;;;; `C-z` is by default bound to `suspend-frame`, but we want it as a
;;;; prefix key.

(global-unset-key (kbd "C-z"))

;;;; ___________________________________________________________________________

(provide 'nomis-disable-c-z)
