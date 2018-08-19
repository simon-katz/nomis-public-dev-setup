;;;; fci-mode (fill-column-indicator)

;;;; See http://www.emacswiki.org/emacs/FillColumnIndicator
;;;;     https://github.com/alpaker/Fill-Column-Indicator

(defvar nomis/fci-mode-issues
  "
There are some issues, so for now just turn on fci-mode temporarily
when you want it.

The issues:

- It turns off line wrapping.
  For discussion and possible solution see
    https://github.com/alpaker/Fill-Column-Indicator/issues/26

- Problem with linum-mode.
  (Something is mentioned at https://github.com/alpaker/Fill-Column-Indicator
  but there's no detail.)
  I see this: Having both linum-mode and fci-mode sometimes causes
  problems on blank lines: line numbers are not displayed and the
  continuation arrow is displayed, unless the cursor is on the blank line
  in which case all is ok.
  I see this sometimes in .el files and .clj files, and sometimes not.
  Changing the width of a frame can make the problem go away.
  Creating a new frame of the same width as one showing the problem
  can create a new frame that deosn't show the problem.
  Hard to understand exactly what's going on.

- General:
  See https://github.com/alpaker/Fill-Column-Indicator/issues.

- I'm also seeing the indicator disappear for lines longer than 80 chars (like this one).
  And sometimes move right a bit for long but not very long lines.
")

;;;; NOTE For this to work (or not work), you need to install
;;;;      fill-column-indicator.

(require 'nomis-right-margin)

(setq-default fill-column nomis/right-margin-column)

(setq fci-rule-color "#cc99cc")

(provide 'nomis-fci-mode)
