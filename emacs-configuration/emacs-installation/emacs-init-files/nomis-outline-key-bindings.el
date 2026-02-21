;;; Init stuff -- nomis-outline-key-bindings --  -*- lexical-binding: t -*-

;;; Code

;;;; Make RET give us a newline

;; We use TAB to do our cycling. We want RET to give us a newline instead of
;; doing `outline-cycle`, so:

(keymap-unset outline-overlay-button-map "RET" t)

;;;; Key bindings for outline-minor-mode-map, cross-referncing `org-mode` mode and `hs-minor-mode`

;; |-------------------+---------------------------------------------------------+-----------------------------|
;; | Key               | org mode                                                | outline                     |

;; |-------------------+---------------------------------------------------------+-----------------------------|
;; |                   | ---- Expand/collapse ----                               |                             |
;; |                   |                                                         |                             |
;; |                   |                                                         |                             |
;; |                   |                                                         |                             |

;; |                   |                                                         |                             |

;; |                   |                                                         |                             |
;; |                   | - The following keys are copied from org.el.            |                             |
;; |                   | -- TAB key with modifiers                               |                             |
;; | "\C-i"            | norg/cycle                                              |                             |
;; | [(tab)]           | norg/cycle                                              | nomis/outline-tab                            |
;; |                   | -- The following line is necessary under Suse GNU/Linux |                             |
;; | [S-iso-lefttab]   | norg/shifttab                                           |                             |
;; | [(shift tab)]     | norg/shifttab                                           | nomis/outline-dec-children                            |
;; | [backtab]         | norg/shifttab                                           |                             |

(define-key outline-minor-mode-map [tab]               'nomis/outline-tab)
(define-key outline-minor-mode-map [S-tab]             'nomis/outline-dec-children)
(define-key outline-minor-mode-map (kbd "H-o <tab>")   'bicycle-cycle-global)

;; |-------------------+---------------------------------------------------------+---------------------------------------------------|
;; |                   | ---- Movement ----                                      |                                                   |


;; |-------------------+---------------------------------------------------------+-----------------------------|
;; |                   | ---- Movement + expand/collapse ----                    |                             |


;; |                   |                                                         |                             |
;; | (kbd "C-H-,")     | norg/backward-heading/any-level                         | nomis/outline-previous-heading |
;; | (kbd "C-H-.")     | norg/forward-heading/any-level                          | nomis/outline-next-heading  |

(define-key outline-minor-mode-map (kbd "C-H-,")       'nomis/outline-previous-heading)
(define-key outline-minor-mode-map (kbd "C-H-.")       'nomis/outline-next-heading)

;; | (kbd "C-H-M-,")   | norg/backward-heading/any-level/set-tree+body           |                             |
;; | (kbd "C-H-M-.")   | norg/forward-heading/any-level/set-tree+body            |                             |
;; |                   |                                                         |                             |
;; | (kbd "C-H-<")     | (*)                                                     |                             |
;; | (kbd "C-H->")     | (*)                                                     |                             |
;; | (kbd "C-H-M-<")   | (*)                                                     |                             |
;; | (kbd "C-H-M->")   | (*)                                                     |                             |
;; |-------------------+---------------------------------------------------------+-----------------------------|

;; (*) No real meaning -- with the M we are already crossing parent levels

;;;; Other outline-minor-mode key bindings

;; outline-minor-mode
;; outline-minor-faces-mode


;; (define-key outline-minor-mode-map (kbd "...") '...)

(define-key outline-minor-mode-map (kbd "H-o p")       'outline-previous-visible-heading)
(define-key outline-minor-mode-map (kbd "H-o n")       'outline-next-visible-heading)
(define-key outline-minor-mode-map (kbd "H-o u")       'outline-up-heading)


(define-key outline-minor-mode-map (kbd "C-S-<right>") 'outline-demote)
(define-key outline-minor-mode-map (kbd "C-S-<left>")  'outline-promote)
(define-key outline-minor-mode-map (kbd "C-S-<up>")    'outline-move-subtree-up)
(define-key outline-minor-mode-map (kbd "C-S-<down>")  'outline-move-subtree-down)

(define-key outline-minor-mode-map (kbd "M-RET")       'outline-insert-heading)

(define-key outline-minor-mode-map (kbd "H-o a")       'outline-show-all) ; TODO: We will have `H-M-=` for `nomis/tree/show-children-from-all-roots/fully-expand`.

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
