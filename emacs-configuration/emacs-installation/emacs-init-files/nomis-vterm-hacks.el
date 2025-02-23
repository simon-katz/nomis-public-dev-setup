;;; nomis-vterm-hacks.el --- vterm hacks -*- lexical-binding: t; -*-

;;;; ___________________________________________________________________________

;;;; :possible-open-source-contribution `nomis/vterm-null-arg-gives-new-buffer`

(cond
 ((member (pkg-info-package-version 'vterm)
          '((20210804 405)
            (20210908 640)
            (20230417 424)
            (20241218 331)))

  ;; Special behaviour for `vterm--internal` when arg is null:
  ;; - Always create a new buffer.
  ;; - Use numbered buffers so that you can use a numeric prefix arg to switch
  ;;   to any buffer.

  (defvar *nomis/-vterm-force-new-buffer?* nil)

  (advice-add
   'vterm--internal
   :around
   (lambda (orig-fun pop-to-buf-fun &optional arg)
     (let* ((*nomis/-vterm-force-new-buffer?* (or (null arg)
                                                  (not (or (numberp arg)
                                                           (stringp arg))))))
       (funcall orig-fun pop-to-buf-fun arg)))
   '((name . nomis/vterm-null-arg-gives-new-buffer)))

  (advice-add
   'get-buffer-create
   :around
   (lambda (orig-fun &rest args)
     (if (not *nomis/-vterm-force-new-buffer?*)
         (apply orig-fun args)
       (cl-flet ((n->buffer-name (n)
                                 (format "%s<%d>"
                                         vterm-buffer-name
                                         n)))
         (let* ((buffer-name (cl-loop for i from 1
                                      for buffer-name = (n->buffer-name i)
                                      unless (get-buffer buffer-name)
                                      return buffer-name)))
           (funcall orig-fun buffer-name)))))
   '((name . nomis/vterm-null-arg-gives-new-buffer))))

 (t
  (message-box
   "You need to fix `nomis/vterm-null-arg-gives-new-buffer` for this version of vterm.")))

(provide 'nomis-vterm-hacks)
