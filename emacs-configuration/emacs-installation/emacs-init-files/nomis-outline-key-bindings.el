;;; nomis-outline-key-bindings.el ---  -*- lexical-binding: t; -*-

;;; Code:

;;;; Requires

(require 'outline)

;;;; Make RET give us a newline

;; We use TAB to do our cycling. We want RET to give us a newline instead of
;; doing `outline-cycle`, so:

(keymap-unset outline-overlay-button-map "RET" t)

;;;; Other outline-minor-mode key bindings

(define-key outline-minor-mode-map (kbd "H-o f")       'outline-minor-faces-mode)

(define-key outline-minor-mode-map (kbd "H-o p")       'outline-previous-visible-heading)
(define-key outline-minor-mode-map (kbd "H-o n")       'outline-next-visible-heading)
(define-key outline-minor-mode-map (kbd "H-o u")       'outline-up-heading)

;; These key bindings match the equivalent `org-mode` built-in key bindings.
(define-key outline-minor-mode-map (kbd "M-S-<right>") 'outline-demote)
(define-key outline-minor-mode-map (kbd "M-S-<left>")  'outline-promote)
(define-key outline-minor-mode-map (kbd "M-S-<up>")    'outline-move-subtree-up)
(define-key outline-minor-mode-map (kbd "M-S-<down>")  'outline-move-subtree-down)

(define-key outline-minor-mode-map (kbd "M-RET")       'outline-insert-heading)

;; TODO: Temporary, until we have `H-M-=` for
;;       `nomis/tree/show-children-from-all-roots/fully-expand`.
(define-key outline-minor-mode-map (kbd "H-o a")       'nomis/outline/show-all)

;; TODO: Maybe add key bindings for the following, and maybe for some other
;;       `outline` commands.

;; | outline-hide-sublevels                    | H-o C-q |             |
;; | outline-show-branches                     | H-o C-k |             |
;; | outline-hide-leaves                       | H-o C-l |             |
;; | outline-show-entry                        | H-o C-e |             |
;; | outline-hide-entry                        | H-o C-c |             |
;; | outline-hide-body                         | H-o C-t |             |
;; | outline-hide-subtree                      | H-o C-d |             |
;; | outline-show-subtree                      | H-o C-s |             |
;; | outline-show-children                     | H-o TAB |             |
;; | outline-hide-by-heading-regexp            | H-o / h |             |
;; | outline-show-by-heading-regexp            | H-o / s |             |

;; | outline-mark-subtree                      | H-o @   |             |
;; | outline-headers-as-kill                   |         |             |
;; | outline-cycle                             |         |             |
;; | outline-cycle-buffer                      |         |             |

;;; End

(provide 'nomis-outline-key-bindings)
