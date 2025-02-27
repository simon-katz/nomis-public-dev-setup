;;; nomis-buffer-menu.el --- Buffer menu tailoring -*- lexical-binding: t -*-

;;;; ___________________________________________________________________________

(cond ((member emacs-version
               '("28.1"
                 "28.2"
                 "29.4"
                 "30.1"))
       (setq Buffer-menu-name-width
             (lambda (buffers)
               ;; Code taken from `Buffer-menu--dynamic-name-width` and hacked.
               (max 19
                    ;; This gives 19 on an 80 column window, and take up
                    ;; proportionally more space as the window widens.
                    (min (truncate (/ (window-width) (cl-case :nomis-hack
                                                       (:original 4.2)
                                                       (:nomis-hack 1.2))))
                         (apply #'max 0 (mapcar (lambda (b)
                                                  (length (buffer-name b)))
                                                buffers)))))))
      (t
       (message-box
        "You need to check `buffer-menu--dynamic-name-width` for this version of Emacs.")))


;;;; ___________________________________________________________________________

(provide 'nomis-buffer-menu)
