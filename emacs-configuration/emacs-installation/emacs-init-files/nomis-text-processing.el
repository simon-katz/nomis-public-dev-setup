;;; nomis-text-processing.el --- text-processing support -*- lexical-binding: t -*-

;;;; ___________________________________________________________________________

(add-to-list 'fill-nobreak-predicate 'fill-single-word-nobreak-p)
(add-to-list 'fill-nobreak-predicate 'fill-single-char-nobreak-p)

;;;; ___________________________________________________________________________

(provide 'nomis-text-processing)
