;;;; Init stuff -- nomis-ielm-persistent-history  -*- lexical-binding: t; -*-

;;;; ___________________________________________________________________________
;;;; Generic stuff

;;;; Copied, with more hacking, from  https://oleksandrmanzyuk.wordpress.com/2011/10/23/a-persistent-command-history-in-emacs/

(require 'ielm)

(defun nomis/comint-write-history-on-exit (process event)
  (comint-write-input-ring)
  (let ((buf (process-buffer process)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (insert (format "\nProcess %s %s" process event))))))

(defun nomis/turn-on-comint-history ()
  (let ((process (get-buffer-process (current-buffer))))
    (when process
      (setq comint-input-ring-file-name
            (format "~/.emacs.d/inferior-%s-history"
                    (process-name process)))
      (comint-read-input-ring)
      (set-process-sentinel process
                            #'nomis/comint-write-history-on-exit))))

(add-hook 'kill-buffer-hook 'comint-write-input-ring)

(defun nomis/mapc-buffers (fn)
  (mapc (lambda (buffer)
          (with-current-buffer buffer
            (funcall fn)))
        (buffer-list)))

(defun nomis/comint-write-input-ring-all-buffers ()
  (nomis/mapc-buffers 'comint-write-input-ring))

(add-hook 'kill-emacs-hook 'nomis/comint-write-input-ring-all-buffers)

;;;; ___________________________________________________________________________
;;;; Persistent ielm history

(add-hook 'ielm-mode-hook 'nomis/turn-on-comint-history)

;;;; ___________________________________________________________________________

(provide 'nomis-ielm-persistent-history)
