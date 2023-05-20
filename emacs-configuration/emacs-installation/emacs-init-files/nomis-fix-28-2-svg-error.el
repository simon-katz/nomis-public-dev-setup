;;;; Init stuff -- nomis-fix-28-2-svg-error --  -*- lexical-binding: t -*-

;;;; Fix problem "Invalid image type ‘svg’" errors.
;;;; See https://emacs.stackexchange.com/questions/74289/emacs-28-2-error-in-macos-ventura-image-type-invalid-image-type-svg

;;;; This also fixes auto-dim behaving badly.

(when (equal emacs-version "28.2")
  (add-to-list 'image-types 'svg))

(provide 'nomis-fix-28-2-svg-error)
