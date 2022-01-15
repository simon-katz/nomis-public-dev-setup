;;;; nomis-lightweight-objects.el --- Lightweight objects ---  -*- lexical-binding: t -*-

;;;; ___________________________________________________________________________

;; TODO: Generalise (need args for `make-hash-table`) or find something that
;;       does this (/eg/ `dash` or something like it).

(defun $$ (&rest kv-pairs)
  (let* ((ht (make-hash-table)))
    (cl-loop for (k v) on kv-pairs by #'cddr
             do (puthash k v ht))
    ht))

(defun $ (k m)
  (map-elt m k))

;;;; ___________________________________________________________________________

(provide 'nomis-lightweight-objects)
