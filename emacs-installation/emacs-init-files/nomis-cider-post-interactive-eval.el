;;;; Init stuff -- nomis-cider-post-interactive-eval ---  -*- lexical-binding: t -*-

;;;; ___________________________________________________________________________
;;;; Design Notes

;;;; This provides a way to specify code to run after
;;;; `cider-eval-defun-at-point` (C-M-x), `cider-load-file` (C-c C-l), and
;;;; similar functions.

;;;; The inspiration for this is to be able to refresh a CLJS UI after these
;;;; commands, but we might as well add similar functionality for CLJ while we
;;;; are at it.

;;;; The first idea might be to do the post-processing in :after advice on
;;;; `cider-interactive-eval` and `cider-load-buffer`, but that doesn't work
;;;; because things are asynchronous. So we have to do the post-processing
;;;; by wrapping callbacks.

;;;; TODO Make this work when entering forms in a REPL buffer.

;;;; ___________________________________________________________________________

(require 'dash)
(require 'cl)
(require 'nomis-memoize)

;;;; ___________________________________________________________________________

;;;; TODO Change this so that user specifies what function to call -- one for
;;;;      CLJ and one for CLJS.

(defvar -nomis/cider/post-interactive-eval/hook-name
  "post-interactive-eval-hook")

(defvar -nomis/cider/post-interactive-eval/clj-ns-name-strings
  '("user"
    "dev"))

(defvar -nomis/cider/post-interactive-eval/cljs-ns-name-strings
  '("cljs.user"))

(defun -nomis/cider/post-interactive-eval/forms-to-eval (repl-type)
  (cl-loop
   for ns-name in (ecase repl-type
                    (clj
                     -nomis/cider/post-interactive-eval/clj-ns-name-strings)
                    (cljs
                     -nomis/cider/post-interactive-eval/cljs-ns-name-strings))
   collect (let* ((symbol-name
                   (format "%s/%s"
                           ns-name
                           -nomis/cider/post-interactive-eval/hook-name)))
             (format "(let [v (resolve '%s)] (when v (v)))"
                     symbol-name))))

;;;; ___________________________________________________________________________

(cond ; nomis/cider/post-interactive-eval
 ((member (cider-version)
          '("CIDER 0.23.0 (Lima)"
            "CIDER 0.24.0snapshot"))

  (defvar *-nomis/cider/post-interactive-eval/do-advice?* t)

  (cl-flet*
      ((check-current-buffer
        (buffer)
        (assert (eql buffer (current-buffer))
                nil
                "Unexpected change of buffer"))

       (running-message
        (form repl-buffer)
        (let ((inhibit-message t))
          (message "nomis/cider/post-interactive-eval Running %S in buffer %S"
                   form (buffer-name repl-buffer))))

       (dup-tried-form-message
        (form)
        (message "nomis/cider/post-interactive-eval DUP TRIED FORM: %s"
                 form))

       (repl-buffer->post-forms
        (repl-buffer)
        (-> repl-buffer
            cider-repl-type
            -nomis/cider/post-interactive-eval/forms-to-eval))

       (run-post-form
        (form repl-buffer)
        (let ((*-nomis/cider/post-interactive-eval/do-advice?* nil))
          (let ((c
                 ;; Don't overwrite the result in the minibuffer (or is it the
                 ;; echo area?). For CLJS, errors are reported in eg the browser
                 ;; console.
                 (cider-stdin-handler)))
            (check-current-buffer repl-buffer)
            (running-message form repl-buffer)
            (cider-interactive-eval form c))))

       (wrap-update-ui
        (callback)
        (let ((do-post-stuff
               (let ((tried-forms '()))
                 (nomis/memoize
                  ;; Memoize because the callback is called multiple times for
                  ;; each REPL, but we only want to do the post-processing once
                  ;; per REPL.
                  (lambda (repl-buffer)
                    (cl-loop for form
                             in (repl-buffer->post-forms repl-buffer)
                             do (if (member form tried-forms)
                                    ;; Sometimes we get here. I've only seen
                                    ;; that just after starting Emacs.
                                    (dup-tried-form-message form)
                                  (progn
                                    (push form tried-forms)
                                    (run-post-form form repl-buffer)))))))))
          (lambda (response)
            (prog1
                (funcall callback response)
              ;; In this callback, the current buffer is a REPL buffer.
              (funcall do-post-stuff (current-buffer)))))))

    (advice-add
     'cider-interactive-eval
     :around
     (lambda (orig-fun form &optional callback bounds additional-params)
       (let* ((buffer (current-buffer)))
         (cl-flet
             ((do-it
               (maybe-hacked-callback)
               (funcall orig-fun form maybe-hacked-callback bounds additional-params)))
           (do-it (if (not *-nomis/cider/post-interactive-eval/do-advice?*)
                      callback
                    (-> (or callback
                            ;; The following is copied from
                            ;; `cider-interactive-eval`.
                            (cider-interactive-eval-handler nil bounds))
                        wrap-update-ui))))))
     '((name . nomis/cider/post-interactive-eval)))

    (advice-add
     'cider-load-buffer
     :around
     (lambda (orig-fun &optional buffer callback)
       (cl-flet ((do-it
                  (hacked-callback)
                  (funcall orig-fun buffer hacked-callback)))
         (do-it (-> (or callback
                        ;; The following is copied from
                        ;; `cider-request:load-file`.
                        (cider-load-file-handler (current-buffer)))
                    wrap-update-ui))))
     '((name . nomis/cider/post-interactive-eval)))))

 (t
  (message-box
   "You need to fix `nomis/cider/post-interactive-eval` for this version of Cider.")))

;;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(when nil ; Code to remove advice when in dev.
  (progn
    (advice-remove 'cider-interactive-eval 'nomis/cider/post-interactive-eval)
    (advice-remove 'cider-load-buffer      'nomis/cider/post-interactive-eval))
  )

;;;; ___________________________________________________________________________

(provide 'nomis-cider-post-interactive-eval)
