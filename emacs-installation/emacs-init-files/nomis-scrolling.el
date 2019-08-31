;;;; nomis-scrolling.el --- Scrolling ---  -*- lexical-binding: t -*-

;;;; ___________________________________________________________________________

(defvar nomis/maintain-line-no-in-window? nil)

(defun nomis/toggle-maintain-line-no-in-window ()
  (interactive)
  (message "nomis/maintain-line-no-in-window? = %s"
           (setq nomis/maintain-line-no-in-window?
                 (not nomis/maintain-line-no-in-window?))))

(defun with-no-visual-line-mode* (fun)
  (let* ((visual-line-mode-was-on? visual-line-mode))
    (when visual-line-mode-was-on?
      (visual-line-mode 0))
    (unwind-protect
        (funcall fun)
      (when visual-line-mode-was-on?
        (visual-line-mode 1)))))

(cl-defmacro with-no-visual-line-mode (&body body)
  `(with-no-visual-line-mode* (lambda () ,@body)))

(defun -nomis/with-maybe-maintain-line-no-in-window* (fun)
  (cl-flet ((do-it () (funcall fun)))
    (if (not nomis/maintain-line-no-in-window?)
        (do-it)
      (with-no-visual-line-mode
       (let* ((line-no-in-window (count-screen-lines (window-start) (point) t)))
         (prog1 (do-it)
           (recenter (1- line-no-in-window))))))))

(cl-defmacro nomis/with-maybe-maintain-line-no-in-window (&body body)
  (declare (indent 0))
  `(-nomis/with-maybe-maintain-line-no-in-window* (lambda () ,@body)))

;;;; ___________________________________________________________________________

(provide 'nomis-scrolling)
