;;; nomis-tree -*- lexical-binding: t -*-

;;; nomis/tree-mode

(defvar nomis/tree-mode-map
  (make-sparse-keymap)
  "Keymap for `nomis/tree-mode'.")

(define-minor-mode nomis/tree-mode
  "Nomis-Tree Minor Mode"
  :group 'nomis/tree
  :keymap nomis/tree-mode-map
  ;; This minor mode exists only as a place to define key bindings, so we don't
  ;; need to do anything when turning on and off.
  )

;;; Turn on the mode

(add-hook 'org-mode-hook 'nomis/tree-mode)
(add-hook 'outline-minor-mode-hook 'nomis/tree-mode)

;; TODO: Do we want this? Or should we be testing whether `hs-minor-mode` is
;;       active when deciding what to do?
;; (add-hook 'hs-minor-mode-hook 'nomis/tree-mode)

;;; End

(provide 'nomis-tree)
