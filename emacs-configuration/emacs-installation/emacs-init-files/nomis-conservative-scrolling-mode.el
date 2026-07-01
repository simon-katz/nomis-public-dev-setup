;;; nomis-conservative-scrolling-mode.el --- Scrolling hacks  -*- lexical-binding: t; -*-

;;; Code:

;;;; Requires

(require 'cl-lib)

;;;;  nomis/conservative-scrolling-mode

;;;;; The mode itself

(defvar nomis/conservative-scrolling-mode-map
  (make-sparse-keymap))

(define-minor-mode nomis/conservative-scrolling-mode
  "Minor mode overriding movement commands with conservative-scrolling versions."
  :global t
  :group 'nomis/scrolling
  :keymap nomis/conservative-scrolling-mode-map)

(add-to-list 'emulation-mode-map-alists
             `((nomis/conservative-scrolling-mode
                . ,nomis/conservative-scrolling-mode-map)))

(define-key global-map (kbd "C-M-ç") ; C-Option-M-c
            #'nomis/conservative-scrolling-mode)

;;;;; nomis/define-conservative-scroller

;; `scroll-conservatively` is read in the post-command redraw phase.
;; The simplest way to set a temporary value is to use `:after` advice that sets
;; up a binding and calls `redisplay` explicitly.

(cl-defmacro nomis/define-conservative-scroller (name key base-command)
  (declare (indent 1))
  `(progn
     (defun ,name (&optional arg)
       ,(format "Call `%s' with conservative scrolling." base-command)
       (interactive "^p")
       (let* ((scroll-conservatively 101))
         ;; Set `this-command` to the base command. This allows e.g. `next-line`
         ;; and `previous-line` to preserve `temporary-goal-column` when
         ;; switching between their conservative-scrolling versions.
         (setq this-command ',base-command)
         (with-suppressed-warnings ((interactive-only ,base-command))
           (,base-command arg))
         (redisplay) ; scroll while let-binding is still alive
         ))
     (define-key nomis/conservative-scrolling-mode-map (kbd ,key)
                 #',name)))

;;;;; Scroll conservatively by line

(nomis/define-conservative-scroller
    nomis/conservative-scrolling/previous-line
  "<up>" previous-line)

(nomis/define-conservative-scroller
    nomis/conservative-scrolling/next-line
  "<down>" next-line)

;;;;; Scroll conservatively by paragraph

(nomis/define-conservative-scroller
    nomis/conservative-scrolling/backward-paragraph
  "C-<up>" backward-paragraph)

(nomis/define-conservative-scroller
    nomis/conservative-scrolling/forward-paragraph
  "C-<down>" forward-paragraph)

;;;;; Scroll conservatively by sexp

(nomis/define-conservative-scroller
    nomis/conservative-scrolling/backward-sexp
  "C-M-b" backward-sexp)

(nomis/define-conservative-scroller
    nomis/conservative-scrolling/forward-sexp
  "C-M-f" forward-sexp)

;;;;; Scroll conservatively by defun

(nomis/define-conservative-scroller
    nomis/conservative-scrolling/beginning-of-defun
  "C-M-a" beginning-of-defun)

(nomis/define-conservative-scroller
    nomis/conservative-scrolling/end-of-defun
  "C-M-e" end-of-defun)

;;;;; Scroll conservatively by list

(nomis/define-conservative-scroller
    nomis/conservative-scrolling/forward-list
  "C-M-n" forward-list)

(nomis/define-conservative-scroller
    nomis/conservative-scrolling/backward-list
  "C-M-p" backward-list)

;;;;; Scroll conservatively for `down-list` and `backward-up-list`

(nomis/define-conservative-scroller
    nomis/conservative-scrolling/down-list
  "C-M-d" down-list)

(nomis/define-conservative-scroller
    nomis/conservative-scrolling/backward-up-list
  "C-M-u" backward-up-list)

;;;;; Scroll conservatively by sentence

(nomis/define-conservative-scroller
    nomis/conservative-scrolling/backward-sentence
  "M-a" backward-sentence)

(nomis/define-conservative-scroller
    nomis/conservative-scrolling/forward-sentence
  "M-e" forward-sentence)

;;; End

(provide 'nomis-conservative-scrolling-mode)
