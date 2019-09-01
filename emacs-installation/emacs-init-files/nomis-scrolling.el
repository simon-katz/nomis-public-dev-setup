;;;; nomis-scrolling.el --- Scrolling ---  -*- lexical-binding: t -*-

;;;; ___________________________________________________________________________

(defvar nomis/scrolling/debug? nil)

(defun nomis/scrolling/debug (format-string &rest format-args)
  (when nomis/scrolling/debug?
    (apply #'message format-string format-args)))

;;;; ___________________________________________________________________________

(defvar nomis/maintain-line-no-in-window? nil)

(defun nomis/toggle-maintain-line-no-in-window ()
  (interactive)
  (message "nomis/maintain-line-no-in-window? = %s"
           (setq nomis/maintain-line-no-in-window?
                 (not nomis/maintain-line-no-in-window?))))

(defun nomis/line-no-in-window ()
  (if (= (point) (window-start))
      ;; Bug in `count-screen-lines`? It's returning 0.
      1
    (count-screen-lines (window-start) (point) t)))

(defun -nomis/with-maybe-maintain-line-no-in-window* (fun)
  (cl-flet* ((report
              (tag)
              (nomis/scrolling/debug "%s: %s" tag (nomis/line-no-in-window))
              ;; (nomis/scrolling/debug "line no = %s" (nomis/line-no-in-window))
              ))
    (nomis/scrolling/debug "____________________________")
    (report "            start")
    (prog1
        (cl-flet* ((do-it () (funcall fun)))
          (if (not nomis/maintain-line-no-in-window?)
              (do-it)
            (let* ((old-line-no (nomis/line-no-in-window)))
              (prog1 (do-it)
                (report "before correction")
                (cl-flet*
                    ((revert-scroll-position
                      ()
                      (let* ((position (point)))
                        (dotimes (_ 2)
                          ;; When near the top of the screen, the first time you do
                          ;; this it doesn't work -- I think because of automatic
                          ;; scrolling. I can't figure out how to stop that happening,
                          ;; but doing things twice instead of once seems to work well.
                          (let* ((new-line-no (nomis/line-no-in-window))
                                 (delta (- new-line-no
                                           old-line-no)))
                            (if (> new-line-no
                                   (window-height))
                                ;; I've seen this in org mode randomly.
                                ;; Seems to be a timing issue.
                                ;; But you can have the problem without getting
                                ;; beyond window-height, so this doesn't really
                                ;; work.
                                (nomis/scrolling/debug "Detected a problem -- not adjusting scroll position")
                              (progn
                                (nomis/scrolling/debug "     scrolling up: %s" delta)
                                (unless (zerop delta)
                                  (ignore-errors
                                    (scroll-up-line delta)))))))
                        (when (/= position (point))
                          ;; We screwed up; I think this happens when we're at the top
                          ;; of the buffer.
                          (nomis/scrolling/debug "Going to position %s" position)
                          (redisplay t) ; point goes to seemingly-random position without this
                          (goto-char position)))))
                  (revert-scroll-position))))))
      (report "              end"))))

(cl-defmacro nomis/with-maybe-maintain-line-no-in-window (&body body)
  (declare (indent 0))
  `(-nomis/with-maybe-maintain-line-no-in-window* (lambda () ,@body)))

;;;; ___________________________________________________________________________

(provide 'nomis-scrolling)
