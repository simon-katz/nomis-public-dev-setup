;;;; nomis-overlay-priorities.el --- nomis-overlay-priorities  tailoring ---  -*- lexical-binding: t -*-

;;;; ___________________________________________________________________________

(setq nomis/ec-base-priority-for-overlays 0) ; the default, but good to see here
(setq hlt-overlays-priority         '(nil . 100000))  ; used in `nomis-idle-highlight-mode`
(setq -nomis/lsp-highlight-priority '(nil . 100001))  ; highest priority -- in-scope stuff

;;;; ___________________________________________________________________________

(provide 'nomis-overlay-priorities)
