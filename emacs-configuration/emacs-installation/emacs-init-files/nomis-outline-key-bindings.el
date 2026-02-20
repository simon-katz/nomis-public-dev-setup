;;; Init stuff -- nomis-outline-key-bindings --  -*- lexical-binding: t -*-

;;; Make RET give us a newline

;; We use TAB to do our cycling. We want RET to give us a newline instead of
;; doing `outline-cycle`, so:

(keymap-unset outline-overlay-button-map "RET" t)

;;; Key bindings for outline-minor-mode-map, cross-referncing `org-mode` mode and `hs-minor-mode`

;; |-------------------+---------------------------------------------------------+----------------------------------------+-----------------------------|
;; | Key               | org mode                                                | hide-show                              | outline                     |

;; |-------------------+---------------------------------------------------------+----------------------------------------+-----------------------------|
;; |                   | ---- Search heading text ----                           |                                        |                             |
;; | (kbd "H-S")       | nomis/org-search-heading-text                           |                                        |                             |
;; | (kbd "H-s")       | nomis/org-search-heading-text-again                     |                                        |                             |

;; |-------------------+---------------------------------------------------------+----------------------------------------+-----------------------------|
;; |                   | ---- Visibility span ----                               |                                        |                             |
;; | (kbd "C-H-'")     | nomis/org-visibility-span/less                          |                                        |                             |
;; | (kbd "C-H-\\")    | nomis/org-visibility-span/more                          |                                        |                             |
;; | (kbd "C-H-M-'")   | nomis/org-visibility-span/set-min                       |                                        |                             |
;; | (kbd "C-H-M-\\")  | nomis/org-visibility-span/set-max                       |                                        | nomis/outline/visibility-span/set-max|

(define-key outline-minor-mode-map (kbd "C-H-M-\\") 'nomis/outline/visibility-span/set-max)

;; |-------------------+---------------------------------------------------------+----------------------------------------+-----------------------------|
;; |                   | ---- Tree only / fat parents and max lineage ----       |                                        |                             |
;; | (kbd "H-M-m")     | norg/show-tree-only                                     |                                        | nomis/outline-show-fat-parents |
;; | (kbd "H-M-M")     |                                                         |                                        | nomis/outline-show-max-lineage |

(define-key outline-minor-mode-map (kbd "H-M-m")        'nomis/outline-show-fat-parents)
(define-key outline-minor-mode-map (kbd "H-M-M")        'nomis/outline-show-max-lineage)


;; |-------------------+---------------------------------------------------------+----------------------------------------+-----------------------------|
;; |                   | ---- Span levels to show ----                           |                                        |                             |
;; | (kbd "H-q H-q s") | norg/set-step-n-levels-to-show                          |                                        |                             |

;; |-------------------+---------------------------------------------------------+----------------------------------------+-----------------------------|
;; |                   | ---- Expand/collapse ----                               |                                        |                             |
;; | (kbd "H-'")       | norg/show-children-from-point/incremental/less          | nomis/hs/adjust/less                   |                             |
;; | (kbd "H-\\")      | norg/show-children-from-point/incremental/more          | nomis/hs/adjust/more                   |                             |
;; | (kbd "H-M-'")     | norg/show-children-from-point/set-min                   | nomis/hs/adjust/set-min                |                             |
;; | (kbd "H-M-\\")    | norg/show-children-from-point/fully-expand              | nomis/hs/adjust/show-all               |                             |
;; |                   |                                                         |                                        |                             |
;; | (kbd "H-\"")      | norg/show-children-from-root/incremental/less           |                                        |                             |
;; | (kbd "H-")        | norg/show-children-from-root/incremental/more           |                                        |                             |
;; | (kbd "H-M-\"")    | norg/show-children-from-root/set-min                    |                                        |                             |
;; | (kbd "H-M-")      | norg/show-children-from-root/fully-expand               |                                        |                             |
;; |                   |                                                         |                                        |                             |
;; | (kbd "H-[")       | norg/show-children-from-parent/incremental/less         |                                        |                             |
;; | (kbd "H-]")       | norg/show-children-from-parent/incremental/more         |                                        |                             |
;; | (kbd "H-M-[")     | norg/show-children-from-parent/set-min                  | nomis/hs/adjust/set-min-for-top-level  |                             |
;; | (kbd "H-M-]")     | norg/show-children-from-parent/fully-expand             | nomis/hs/adjust/show-all-for-top-level |                             |
;; |                   |                                                         |                                        |                             |
;; | (kbd "H--")       | norg/show-children-from-all-roots/incremental/less      |                                        |                             |
;; | (kbd "H-=")       | norg/show-children-from-all-roots/incremental/more      |                                        |                             |
;; | (kbd "H-M--")     | norg/show-children-from-all-roots/set-min               | nomis/hs/hide-all                      |                             |
;; | (kbd "H-M-=")     | norg/show-children-from-all-roots/fully-expand          | nomis/hs/show-all                      | Note: "H-o a" for outline-show-all |
;; |                   |                                                         |                                        |                             |
;; | (kbd "H-q H-q ]") | norg/show-children-from-root/to-current-level           |                                        |                             |
;; | (kbd "H-q H-q =") | norg/show-children-from-all-roots/to-current-level      |                                        |                             |
;; |                   |                                                         |                                        |                             |
;; |                   | - The following keys are copied from org.el.            |                                        |                             |
;; |                   | -- TAB key with modifiers                               |                                        |                             |
;; | "\C-i"            | norg/cycle                                              |                                        |                             |
;; | [(tab)]           | norg/cycle                                              |                                        | nomis/outline-tab                            |
;; |                   | -- The following line is necessary under Suse GNU/Linux |                                        |                             |
;; | [S-iso-lefttab]   | norg/shifttab                                           |                                        |                             |
;; | [(shift tab)]     | norg/shifttab                                           |                                        | nomis/outline-dec-children                            |
;; | [backtab]         | norg/shifttab                                           |                                        |                             |

(define-key outline-minor-mode-map [tab]               'nomis/outline-tab)
(define-key outline-minor-mode-map [S-tab]             'nomis/outline-dec-children)
(define-key outline-minor-mode-map (kbd "H-o <tab>")   'bicycle-cycle-global)

;; |-------------------+---------------------------------------------------------+----------------------------------------+---------------------------------------------------|
;; |                   | ---- Movement ----                                      |                                        |                                                   |
;; | (kbd "H-,")       | norg/backward-heading-same-level                        |                                        | nomis/outline-previous-sibling                    |
;; | (kbd "H-.")       | norg/forward-heading-same-level                         |                                        | nomis/outline-next-sibling                        |
;; | (kbd "H-<")       | norg/backward-heading-same-level/allow-cross-parent     |                                        | nomis/outline-previous-sibling/allow-cross-parent |
;; | (kbd "H->")       | norg/forward-heading-same-level/allow-cross-parent      |                                        | nomis/outline-next-sibling/allow-cross-parent     |

(define-key outline-minor-mode-map (kbd "H-,")         'nomis/outline-previous-sibling)
(define-key outline-minor-mode-map (kbd "H-.")         'nomis/outline-next-sibling)
(define-key outline-minor-mode-map (kbd "H-<")         'nomis/outline-previous-sibling/allow-cross-parent)
(define-key outline-minor-mode-map (kbd "H->")         'nomis/outline-next-sibling/allow-cross-parent)

;; |-------------------+---------------------------------------------------------+----------------------------------------+-----------------------------|
;; |                   | ---- Movement + expand/collapse ----                    |                                        |                             |
;; | (kbd "H-M-,")     | norg/step-backward                                      | *WAS* nomis/hs/step-backward           | nomis/outline-step-backward |
;; | (kbd "H-M-.")     | norg/step-forward                                       | *WAS* nomis/hs/step-forward            | nomis/outline-step-forward  |
;; | (kbd "H-M-<")     | norg/step-backward/allow-cross-parent                   |                                        | nomis/outline-step-backward/allow-cross-parent |
;; | (kbd "H-M->")     | norg/step-forward/allow-cross-parent                    |                                        | nomis/outline-step-forward/allow-cross-parent |

(define-key outline-minor-mode-map (kbd "H-M-,")       'nomis/outline-step-backward)
(define-key outline-minor-mode-map (kbd "H-M-.")       'nomis/outline-step-forward)
(define-key outline-minor-mode-map (kbd "H-M-<")       'nomis/outline-step-backward/allow-cross-parent)
(define-key outline-minor-mode-map (kbd "H-M->")       'nomis/outline-step-forward/allow-cross-parent)

;; |                   |                                                         |                                        |                             |
;; | (kbd "C-H-,")     | norg/backward-heading/any-level                         |                                        | nomis/outline-previous-heading |
;; | (kbd "C-H-.")     | norg/forward-heading/any-level                          |                                        | nomis/outline-next-heading  |

(define-key outline-minor-mode-map (kbd "C-H-,")       'nomis/outline-previous-heading)
(define-key outline-minor-mode-map (kbd "C-H-.")       'nomis/outline-next-heading)

;; | (kbd "C-H-M-,")   | norg/backward-heading/any-level/set-tree+body           |                                        |                             |
;; | (kbd "C-H-M-.")   | norg/forward-heading/any-level/set-tree+body            |                                        |                             |
;; |                   |                                                         |                                        |                             |
;; | (kbd "C-H-<")     | (*)                                                     |                                        |                             |
;; | (kbd "C-H->")     | (*)                                                     |                                        |                             |
;; | (kbd "C-H-M-<")   | (*)                                                     |                                        |                             |
;; | (kbd "C-H-M->")   | (*)                                                     |                                        |                             |
;; |                   |                                                         |                                        |                             |
;; | (kbd "H-q H-q m") | nomis/scrolling/toggle-maintain-line-no-in-window       | Same as <-                             |                             |

;; |-------------------+---------------------------------------------------------+----------------------------------------+-----------------------------|

;; (*) No real meaning -- with the M we are already crossing parent levels

;;; Other outline-minor-mode key bindings

;; outline-minor-mode
;; outline-minor-faces-mode


;; (define-key outline-minor-mode-map (kbd "...") '...)

(define-key outline-minor-mode-map (kbd "H-o p")     'outline-previous-visible-heading)
(define-key outline-minor-mode-map (kbd "H-o n")     'outline-next-visible-heading)
(define-key outline-minor-mode-map (kbd "H-o u")     'outline-up-heading)


(define-key outline-minor-mode-map (kbd "C-S-<right>") 'outline-demote)
(define-key outline-minor-mode-map (kbd "C-S-<left>")  'outline-promote)
(define-key outline-minor-mode-map (kbd "C-S-<up>")    'outline-move-subtree-up)
(define-key outline-minor-mode-map (kbd "C-S-<down>")  'outline-move-subtree-down)

(define-key outline-minor-mode-map (kbd "M-RET")       'outline-insert-heading)

(define-key outline-minor-mode-map (kbd "H-o a")     'outline-show-all)

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
