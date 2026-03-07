;;; nomis-tree-outline-2.el ---  -*- lexical-binding: t; -*-

;; TODO: The names in this file should be `nomis/tree-outline-2/xxxx`.

;; TODO: Maybe move all this to `nomis-tree`. And similarly maybe move
;;       contents of `norg` to `nomis-tree`.

;;; Code:

;;;; Requires

(require 'nomis-msg)
(require 'outline)

;;;; API

;;;;; nomis/tree/outline/show-all

;; TODO: Temporary, until we have
;;       `nomis/tree/show-children-from-all-roots/fully-expand`.
(defun nomis/tree/outline/show-all ()
  (interactive)
  (outline-show-all)
  (nomis/msg/pulse-buffer))

;;; End

(provide 'nomis-tree-outline-2)
