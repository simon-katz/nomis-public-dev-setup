;;;; Init stuff -- ace jump mode.

;;;; See:
;;;; - https://github.com/winterTTr/ace-jump-mode
;;;; - https://github.com/winterTTr/ace-jump-mode/wiki/AceJump-FAQ

;;;; ___________________________________________________________________________

(require 'ace-jump-mode)

(ace-jump-mode-enable-mark-sync)

(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)

(define-key global-map (kbd "C-x SPC") 'ace-jump-mode-pop-mark)

;;;; ___________________________________________________________________________

(provide 'nomis-ace-jump-mode)
