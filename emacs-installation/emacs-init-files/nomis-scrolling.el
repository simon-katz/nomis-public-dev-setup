;;;; nomis-scrolling.el --- Scrolling ---  -*- lexical-binding: t -*-

;;;; ___________________________________________________________________________

(defun -nomis/with-maintain-line-no-in-window* (fun)
  (let* ((line-no-in-window (count-screen-lines (window-start) (point))))
    (prog1 (funcall fun)
      (recenter line-no-in-window))))

(cl-defmacro nomis/with-maintain-line-no-in-window (&body body)
  (declare (indent 0))
  `(-nomis/with-maintain-line-no-in-window* (lambda () ,@body)))

;;;; ___________________________________________________________________________

(provide 'nomis-scrolling)
