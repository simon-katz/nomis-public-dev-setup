;;; Init stuff -- nomis-outline-key-bindings --  -*- lexical-binding: t -*-

;;; Make RET give us a newline

;; We use TAB to do our cycling. We want RET to give us a newline instead of
;; doing `outline-cycle`, so:

(keymap-unset outline-overlay-button-map "RET" t)

;;; Key bindings for org mode and hide/show

;; |-------------------+---------------------------------------------------------+----------------------------------------+-----------------------------|
;; | Key               | org mode                                                | hide/show                              | outline                     |

;; |-------------------+---------------------------------------------------------+----------------------------------------+-----------------------------|
;; |                   | ---- Help ----                                          |                                        |                             |
;; | (kbd "H-q H-q /") | nomis/org/pop-up-navigation-and-cycling-help            |                                        |                             |

;; |-------------------+---------------------------------------------------------+----------------------------------------+-----------------------------|
;; |                   | ---- Following links ----                               |                                        |                             |
;; | (kbd "M-.")       | org-open-at-point                                       |                                        |                             |
;; | (kbd "M-,")       | org-mark-ring-goto                                      |                                        |                             |

;; |-------------------+---------------------------------------------------------+----------------------------------------+-----------------------------|
;; |                   | ---- Search heading text ----                           |                                        |                             |
;; | (kbd "H-S")       | nomis/org-search-heading-text                           |                                        |                             |
;; | (kbd "H-s")       | nomis/org-search-heading-text-again                     |                                        |                             |

;; |-------------------+---------------------------------------------------------+----------------------------------------+-----------------------------|
;; |                   | ---- Visibility span ----                               |                                        |                             |
;; | (kbd "C-H-'")     | nomis/org-visibility-span/less                          |                                        |                             |
;; | (kbd "C-H-\\")    | nomis/org-visibility-span/more                          |                                        |                             |
;; | (kbd "C-H-M-'")   | nomis/org-visibility-span/set-min                       |                                        |                             |
;; | (kbd "C-H-M-\\")  | nomis/org-visibility-span/set-max                       |                                        |                             |
;; |                   |                                                         |                                        |                             |
;; | (kbd "H-M-m")     | norg/show-tree-only                                     |                                        |                             |
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
;; | (kbd "H-M-=")     | norg/show-children-from-all-roots/fully-expand          | nomis/hs/show-all                      |                             |
;; |                   |                                                         |                                        |                             |
;; | (kbd "H-q H-q ]") | norg/show-children-from-root/to-current-level           |                                        |                             |
;; | (kbd "H-q H-q =") | norg/show-children-from-all-roots/to-current-level      |                                        |                             |
;; |                   |                                                         |                                        |                             |
;; |                   | - The following keys are copied from org.el.            |                                        |                             |
;; |                   | -- TAB key with modifiers                               |                                        |                             |
;; | "\C-i"            | norg/cycle                                              |                                        |                             |
;; | [(tab)]           | norg/cycle                                              |                                        |                             |
;; |                   | -- The following line is necessary under Suse GNU/Linux |                                        |                             |

;; | [S-iso-lefttab]   | norg/shifttab                                           |                                        |                             |
;; | [(shift tab)]     | norg/shifttab                                           |                                        |                             |
;; | [backtab]         | norg/shifttab                                           |                                        |                             |
(define-key outline-minor-mode-map [tab] 'nomis/outline-cycle-or-indent-or-complete)

(define-key outline-minor-mode-map [C-tab] 'bicycle-cycle)
(define-key outline-minor-mode-map [S-tab] 'bicycle-cycle-global)

;; |-------------------+---------------------------------------------------------+----------------------------------------+-----------------------------|
;; |                   | ---- Movement ----                                      |                                        |                             |
;; | (kbd "H-,")       | norg/backward-heading-same-level                        |                                        |                             |
;; | (kbd "H-.")       | norg/forward-heading-same-level                         |                                        |                             |
;; | (kbd "H-<")       | norg/backward-heading-same-level/allow-cross-parent     |                                        |                             |
;; | (kbd "H->")       | norg/forward-heading-same-level/allow-cross-parent      |                                        |                             |

;; |-------------------+---------------------------------------------------------+----------------------------------------+-----------------------------|
;; |                   | ---- Movement + expand/collapse ----                    |                                        |                             |
;; | (kbd "H-M-,")     | norg/step-backward                                      |                                        |                             |
;; | (kbd "H-M-.")     | norg/step-forward                                       |                                        |                             |
;; | (kbd "H-M-<")     | norg/step-backward/allow-cross-parent                   |                                        |                             |
;; | (kbd "H-M->")     | norg/step-forward/allow-cross-parent                    |                                        |                             |
;; |                   |                                                         |                                        |                             |
;; | (kbd "C-H-,")     | norg/backward-heading/any-level                         |                                        |                             |
;; | (kbd "C-H-.")     | norg/forward-heading/any-level                          |                                        |                             |
;; | (kbd "C-H-M-,")   | norg/backward-heading/any-level/set-tree+body           |                                        |                             |
;; | (kbd "C-H-M-.")   | norg/forward-heading/any-level/set-tree+body            |                                        |                             |
;; |                   |                                                         |                                        |                             |
;; | (kbd "C-H-<")     | (*)                                                     |                                        |                             |
;; | (kbd "C-H->")     | (*)                                                     |                                        |                             |
;; | (kbd "C-H-M-<")   | (*)                                                     |                                        |                             |
;; | (kbd "C-H-M->")   | (*)                                                     |                                        |                             |
;; |                   |                                                         |                                        |                             |
;; | (kbd "H-q H-q m") | nomis/scrolling/toggle-maintain-line-no-in-window       |                                        |                             |

;; |-------------------+---------------------------------------------------------+----------------------------------------+-----------------------------|

;; (*) No real meaning -- with the M we are already crossing parent levels

;;; End

(provide 'nomis-outline-key-bindings)
