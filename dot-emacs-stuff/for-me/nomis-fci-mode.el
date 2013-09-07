;;;; fci-mode (fill-column-indicator)

;;;; http://www.emacswiki.org/emacs/FillColumnIndicator
;;;; https://github.com/alpaker/Fill-Column-Indicator
;;;; https://github.com/alpaker/Fill-Column-Indicator/issues
;;;; It turns off my line wrapping. Annoying.
;;;; For discussion and possible solution see
;;;;   https://github.com/alpaker/Fill-Column-Indicator/issues/26
;;;; 
;;;; Problem with linum-mode in .el files. (Is it only .el files?).
;;;; Something is mentioned at https://github.com/alpaker/Fill-Column-Indicator
;;;; but there's no detail.
;;;; I see this: Having both linum-mode and fci-mode in .el files causes
;;;; problems on blank lines: line numbers are not displayed and the
;;;; continuation arrow is displayed, unless the cursor is on the blank line
;;;; in which case all is ok.

(setq-default fill-column nomis-right-margin)

(defun fci-mode-off () (fci-mode -1))

(provide 'nomis-fci-mode)
