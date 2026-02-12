;;; Init stuff -- nomis-outline-key-bindings --  -*- lexical-binding: t -*-

;;; Make RET give us a newline

;; We use TAB to do our cycling. We want RET to give us a newline instead of
;; doing `outline-cycle`, so:

(keymap-unset outline-overlay-button-map "RET" t)

;;; outline-minor-mode

(with-eval-after-load 'bicycle
  (define-key outline-minor-mode-map [C-tab] 'bicycle-cycle)
  (define-key outline-minor-mode-map [S-tab] 'bicycle-cycle-global))

(define-key outline-minor-mode-map [tab]
            'nomis/outline-cycle-or-indent-or-complete)

;;; End

(provide 'nomis-outline-key-bindings)
