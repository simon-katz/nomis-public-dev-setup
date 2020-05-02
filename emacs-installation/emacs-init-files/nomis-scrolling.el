;;;; nomis-scrolling.el --- Scrolling ---  -*- lexical-binding: t -*-

;;;; ___________________________________________________________________________

(defvar nomis/scrolling/debug? nil)

(defun nomis/scrolling/debug (format-string &rest format-args)
  (when nomis/scrolling/debug?
    (apply #'message format-string format-args)))

;;;; ___________________________________________________________________________

(defvar *nomis/maintain-line-no-in-window?* nil)

(defun nomis/toggle-maintain-line-no-in-window ()
  (interactive)
  (message "*nomis/maintain-line-no-in-window?* = %s"
           (setq *nomis/maintain-line-no-in-window?*
                 (not *nomis/maintain-line-no-in-window?*))))

(defun nomis/line-no-in-window ()
  (if (= (point) (window-start))
      ;; Bug in `count-screen-lines`? It's returning 0.
      1
    (count-screen-lines (window-start) (point) t)))

(defun -nomis/with-maybe-maintain-line-no-in-window* (fun)
  (cl-flet* ((do-it () (funcall fun)))
    (if (not *nomis/maintain-line-no-in-window?*)
        (do-it)
      (let* ((old-line-no (nomis/line-no-in-window)))
        (prog1 (do-it)
          ;; Ensure cursor is on screen, so that scrolling doesn't make
          ;; any unwanted adjustments.
          (let* ((recenter-redisplay nil))
            (recenter))
          ;; Reset scroll position
          (ignore-errors
            ;; `ignore-errors` because if we're near the top of the buffer
            ;; we may not be able to do this.
            (scroll-up-line (- (nomis/line-no-in-window)
                               old-line-no))))))))

(cl-defmacro nomis/with-maybe-maintain-line-no-in-window (&body body)
  (declare (indent 0))
  `(-nomis/with-maybe-maintain-line-no-in-window* (lambda () ,@body)))

;;;; ___________________________________________________________________________

(provide 'nomis-scrolling)
