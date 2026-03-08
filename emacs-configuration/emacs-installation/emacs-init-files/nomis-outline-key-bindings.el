;;; nomis-outline-key-bindings.el ---  -*- lexical-binding: t; -*-

;;; Code:

;;;; Requires

(require 'nomis-tree-key-bindings)
(require 'outline)

;;;; Make RET give us a newline

;; We use TAB to do our cycling. We want RET to give us a newline instead of
;; doing `outline-cycle`, so:

(keymap-unset outline-overlay-button-map "RET" t)

;;;; Other outline-minor-mode key bindings

(define-key nomis/tree/kb/map (kbd "f") 'outline-minor-faces-mode)

;; These key bindings match the equivalent `org-mode` built-in key bindings.
(define-key outline-minor-mode-map (kbd "M-S-<right>") 'outline-demote)
(define-key outline-minor-mode-map (kbd "M-S-<left>")  'outline-promote)
(define-key outline-minor-mode-map (kbd "M-S-<up>")    'outline-move-subtree-up)
(define-key outline-minor-mode-map (kbd "M-S-<down>")  'outline-move-subtree-down)

(define-key outline-minor-mode-map (kbd "M-RET")       'outline-insert-heading)

(defvar-keymap nomis/outline/kb/map
  :doc "Keymap for nomis/outline commands.")

(define-key nomis/tree/kb/map (kbd "H-o") nomis/outline/kb/map)

(define-key nomis/outline/kb/map (kbd "c")   'outline-hide-entry)
(define-key nomis/outline/kb/map (kbd "e")   'outline-show-entry)
(define-key nomis/outline/kb/map (kbd "d")   'outline-hide-subtree)
(define-key nomis/outline/kb/map (kbd "s")   'outline-show-subtree)
(define-key nomis/outline/kb/map (kbd "l")   'outline-hide-leaves)
(define-key nomis/outline/kb/map (kbd "k")   'outline-show-branches)
(define-key nomis/outline/kb/map (kbd "i")   'outline-show-children)
(define-key nomis/outline/kb/map (kbd "t")   'outline-hide-body)
(define-key nomis/outline/kb/map (kbd "a")   'outline-show-all)
(define-key nomis/outline/kb/map (kbd "q")   'outline-hide-sublevels)
(define-key nomis/outline/kb/map (kbd "o")   'outline-hide-other)
(define-key nomis/outline/kb/map (kbd "/ h") 'outline-hide-by-heading-regexp)
(define-key nomis/outline/kb/map (kbd "/ s") 'outline-show-by-heading-regexp)
(define-key nomis/outline/kb/map (kbd "@")   'outline-mark-subtree)

;;; End

(provide 'nomis-outline-key-bindings)
