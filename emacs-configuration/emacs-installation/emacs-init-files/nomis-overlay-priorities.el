;;;; nomis-overlay-priorities.el --- nomis-overlay-priorities  tailoring ---  -*- lexical-binding: t -*-

;;;; ___________________________________________________________________________

;;;; Note that the region's overlay has priority `(nil . 100)` -- see
;;;; `redisplay--highlight-overlay-function`. So we want to have values less
;;;; than 100. (I found this out at https://emacs.stackexchange.com/q/17324)

(setq nomis/ec-base-priority-for-overlays 0) ; the default, but good to see here
(setq hlt-overlays-priority         '(nil . 98))  ; used in `nomis-idle-highlight-mode`
(setq -nomis/lsp-highlight-priority '(nil . 99))  ; highest priority -- in-scope stuff

;;;; ___________________________________________________________________________

(provide 'nomis-overlay-priorities)
