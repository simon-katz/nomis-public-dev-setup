;;; nomis-avro.el --- Avro support -*- lexical-binding: t -*-

;;;; ___________________________________________________________________________

;; JSON is a subset of JavaScript, so this will do:
(add-to-list 'auto-mode-alist '("\\.avsc\\'" . js-mode))

;;;; ___________________________________________________________________________

(provide 'nomis-avro)
