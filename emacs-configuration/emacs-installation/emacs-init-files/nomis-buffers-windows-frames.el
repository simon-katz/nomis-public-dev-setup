;;;; Init stuff -- Files.

(require 'cl)

;;;; ___________________________________________________________________________

(cl-defun nomis/find-window-in-frame (buffer-name &optional frame)
  (let* ((frame (or frame
                    (selected-frame)))
         (windows (window-list frame)))
    (cl-find-if (lambda (w)
                  (equal (-> w window-buffer buffer-name)
                         buffer-name))
                windows)))

(defun nomis/find-window-in-any-frame (buffer-name)
  (loop for f in (frame-list)
        for w = (nomis/find-window-in-frame buffer-name f)
        when w
        return w))

(cl-defun nomis/find-window-in-any-frame-pref-this-one (buffer-name
                                                        &optional frame)
  (or (nomis/find-window-in-frame buffer-name frame)
      (nomis/find-window-in-any-frame buffer-name)))

(defun nomis/foreach-buffer-window (buffer f)
  (dolist (w (get-buffer-window-list buffer nil t))
    (with-selected-window w
      (funcall f))))

;;;; ___________________________________________________________________________

(provide 'nomis-buffers-windows-frames)
