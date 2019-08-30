;;;; nomis-popup --- A layer on top of popup  ---  -*- lexical-binding: t -*-

(progn) ; this-stops-hs-hide-all-from-hiding-the-next-comment

;;;; ___________________________________________________________________________
;;;; ____ * Require things

(require 'popup)
(require 'nomis-msg)

;;;; ___________________________________________________________________________
;;;; ____ * nomis/popup/message

(defvar -nomis/popup/most-recent-popup nil)

(defun nomis/popup/message (format-string &rest args)
  (let* ((msg (apply #'format format-string args)))
    (run-at-time 0
                 nil
                 (lambda ()
                   (when (and -nomis/popup/most-recent-popup
                              (popup-live-p -nomis/popup/most-recent-popup))
                     (popup-delete -nomis/popup/most-recent-popup)
                     (setq -nomis/popup/most-recent-popup nil))
                   (let* ((popup
                           (popup-tip msg
                                      :nowait t
                                      :point (save-excursion
                                               (unless (get-char-property
                                                        (point)
                                                        'invisible)
                                                 (ignore-errors
                                                   (previous-line 2)))
                                               (point)))))
                     (setq -nomis/popup/most-recent-popup popup)
                     (run-at-time 1
                                  nil
                                  (lambda ()
                                    (when (popup-live-p popup)
                                      (popup-delete popup)))))))))

(defvar nomis/popup/error-message-prefix "!! ")

(defun nomis/popup/error-message (format-string &rest args)
  (apply #'nomis/popup/message
         (concat nomis/popup/error-message-prefix
                 format-string)
         args)
  (nomis/msg/grab-user-attention/low))

;;;; ___________________________________________________________________________
;;;; * End

(provide 'nomis-popup)
