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


;;;; TODO You've lost the popup message in one direction when at top of window


(defun -nomis/with-maybe-maintain-line-no-in-window* (fun)
  (cl-flet* ((report
              (tag)
              (nomis/scrolling/debug "%s: %s" tag (nomis/line-no-in-window))
              ;; (nomis/scrolling/debug "line no = %s" (nomis/line-no-in-window))
              ))
    (nomis/scrolling/debug "____________________________")
    (report "            start")
    (let* ((can-scroll-down? (> (line-number-at-pos (window-start)) 1)))
      (when can-scroll-down? (scroll-down-line))
      (report "   adjusted start")
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
                        (let* ((position (point))
                               ;; (scroll-conservatively 10000)
                               ;; (scroll-down-aggressively 0.0)
                               )
                          (dotimes (_ 1)
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
                                  (nomis/scrolling/debug "   scrolling up: %s" delta)
                                  (scroll-up-line delta)))))
                          (report "after correction")
                          (when (/= position (point))
                            ;; We screwed up; I think this happens when we're at the top
                            ;; of the buffer.
                            (report "before going to position")
                            (nomis/scrolling/debug "Going to position %s" position)
                            (redisplay t) ; point goes to seemingly-random position without this
                            (goto-char position)
                            (report " after going to position")))))
                    (revert-scroll-position))))))
        (report "     adjusted end")
        (when can-scroll-down? (scroll-up-line))
        (report "              end")))))

(cl-defmacro nomis/with-maybe-maintain-line-no-in-window (&body body)
  (declare (indent 0))
  `(-nomis/with-maybe-maintain-line-no-in-window* (lambda () ,@body)))

;;;; ___________________________________________________________________________

(provide 'nomis-scrolling)
