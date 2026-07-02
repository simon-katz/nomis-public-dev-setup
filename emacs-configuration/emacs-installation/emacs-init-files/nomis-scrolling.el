;;; nomis-scrolling.el --- Scrolling hacks  -*- lexical-binding: t; -*-

;;; Code:

;;;; Requires

(require 'cl-lib)

;;;; nomis/scrolling/-debug

(defvar nomis/scrolling/-debug? nil)

(defun nomis/scrolling/-debug (format-string &rest format-args)
  (let* ((inhibit-message t))
    (when nomis/scrolling/-debug?
      (apply #'message format-string format-args))))

;;;; maintain-line-no-in-window

(defvar nomis/scrolling/maintain-line-no-in-window? nil)

(defun nomis/scrolling/toggle-maintain-line-no-in-window ()
  (interactive)
  (message "nomis/scrolling/maintain-line-no-in-window? = %s"
           (setq nomis/scrolling/maintain-line-no-in-window?
                 (not nomis/scrolling/maintain-line-no-in-window?))))

(defun nomis/scrolling/-line-no-in-window ()
  (if (= (point) (window-start))
      ;; Bug in `count-screen-lines`? It's returning 0.
      1
    (count-screen-lines (window-start) (point) t)))

(defun nomis/scrolling/restore-scroll-position (old-line-no)
  ;; Ensure cursor is on screen, so that scrolling doesn't make
  ;; any unwanted adjustments.
  (let* ((recenter-redisplay nil))
    (recenter))
  ;; Reset scroll position.
  (ignore-errors
    ;; `ignore-errors` because if we're near the top of the buffer we may not be
    ;; able to do this.
    (scroll-up-line (- (nomis/scrolling/-line-no-in-window)
                       old-line-no))))

(defvar nomis/scrolling/-old-line-no nil)

(defun nomis/scrolling/maybe-restore-scroll-position ()
  "Unused. Was needed when we had a `run-at-time` before expanding parents."
  (when (and nomis/scrolling/maintain-line-no-in-window?
             nomis/scrolling/-old-line-no)
    (nomis/scrolling/restore-scroll-position nomis/scrolling/-old-line-no)))

(defun nomis/scrolling/-with-maybe-maintain-line-no-in-window* (fun force?)
  (cl-flet* ((do-it () (funcall fun)))
    (if (not (or force?
                 nomis/scrolling/maintain-line-no-in-window?))
        (progn (setq nomis/scrolling/-old-line-no nil)
               (do-it))
      (let* ((old-line-no (nomis/scrolling/-line-no-in-window)))
        (setq nomis/scrolling/-old-line-no old-line-no)
        (prog1 (do-it)
          (nomis/scrolling/restore-scroll-position old-line-no))))))

(cl-defmacro nomis/scrolling/with-maybe-maintain-line-no-in-window (&body body)
  (declare (indent 0))
  `(nomis/scrolling/-with-maybe-maintain-line-no-in-window* (lambda () ,@body)
                                                            nil))

(cl-defmacro nomis/scrolling/with-force-maintain-line-no-in-window (&body body)
  (declare (indent 0))
  `(nomis/scrolling/-with-maybe-maintain-line-no-in-window* (lambda () ,@body)
                                                            t))

;;;; Improve autoscrolling

;;;;; nomis/define-preserving-scroller

(cl-defmacro nomis/define-preserving-scroller (command)
  (declare (indent 1))
  `(advice-add ,command
               :around
               (lambda (orig-fun &rest args)
                 (let* ((scroll-preserve-screen-position t))
                   (apply orig-fun args)))
               '((name . nomis/scroll-preserve-screen-position))))

;;;;; Maintain screen position on various commands

;; `scroll-preserve-screen-position` is read inside these commands, so we can
;; use `:around` advice to set it. (Contrast with `scroll-conservatively` which
;; is read in the post-command redraw phase.)

(defconst nomis/scrolling/preserve-screen-position-commands
  '(scroll-up-command
    scroll-down-command
    scroll-other-window
    scroll-other-window-down))

(dolist (command nomis/scrolling/preserve-screen-position-commands)
  (nomis/define-preserving-scroller command))

;; (dolist (command nomis/scrolling/preserve-screen-position-commands)
;;   (advice-remove command 'nomis/scroll-preserve-screen-position))

;;; End

(provide 'nomis-scrolling)
