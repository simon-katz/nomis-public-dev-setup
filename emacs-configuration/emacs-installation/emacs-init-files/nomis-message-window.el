;;;; nomis-message-window.el ---  -*- lexical-binding: t -*-

;;;; ___________________________________________________________________________
;;;; nomis/fix-broken-messages-window and undo

(defun nomis/fix-broken-messages-window ()
  "Sometimes the *Messages* buffer stops tailing. When that happens, call
this for a bit of a fix.
This simply moves window-point to the end of the buffer in all windows
that are showing the buffer."
  (interactive)
  (advice-add 'message
              :after
              (lambda (&rest args)
                (with-current-buffer "*Messages*"
                  (goto-char (point-max))
                  (let ((windows (get-buffer-window-list (current-buffer) nil t)))
                    (dolist (w windows)
                      (set-window-point w (point-max))))))
              '((name . goto-end-of-messages-buffer))))

(defun nomis/fix-broken-messages-window/undo ()
  "Undo the effect of `nomis/fix-broken-messages-window`."
  (interactive)
  (advice-remove 'message
                 'goto-end-of-messages-buffer))

;;;; ___________________________________________________________________________
;;;; nomis/clear-messages-buffer

(defun nomis/clear-messages-buffer ()
  "Clear the *Messages* buffer."
  (interactive)
  (with-current-buffer "*Messages*"
    (let* ((was-read-only buffer-read-only))
      (when was-read-only
        (read-only-mode -1))
      (erase-buffer)
      (when was-read-only
        (read-only-mode 1)))))

(define-key messages-buffer-mode-map (kbd "M-k") 'nomis/clear-messages-buffer)

;;;; ___________________________________________________________________________

(provide 'nomis-message-window)
