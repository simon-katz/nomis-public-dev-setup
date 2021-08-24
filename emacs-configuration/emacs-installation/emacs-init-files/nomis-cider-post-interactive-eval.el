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

(defvar nomis/cider/post-interactive-eval/clj-function-name nil
  "The fully-qualified name (as a string) of a function to call after evaluating
a CLJ form, for example \"dev/post-interactive-eval-hook\".")

(defvar nomis/cider/post-interactive-eval/cljs-function-name nil
  "The fully-qualified name (as a string) of a function to call after evaluating
a CLJS form, for example \"cljs.user/post-interactive-eval-hook\".
This can be used, for example, to update UIs after evaluating forms (without
the need to save files so that a file-watcher can spot changes).")

;;;; ___________________________________________________________________________

(defvar *-nomis/cider/post-interactive-eval/do-advice?* t)

(cond ; nomis/cider/post-interactive-eval
 ((member (cider-version)
          '("CIDER 0.23.0 (Lima)"
            "CIDER 0.24.0snapshot"
            "CIDER 0.26.1 (Nesebar)"))

  (cl-flet*
      ((check-current-buffer
        (buffer)
        (assert (eql buffer (current-buffer))
                nil
                "Unexpected change of buffer"))

       (running-message
        (form repl-buffer)
        (let ((inhibit-message t))
          (case :do-not-do-this
            (1 (message "nomis/cider/post-interactive-eval Running %S in buffer %S"
                        form (buffer-name repl-buffer)))
            (2 (message "Doing %s post-interactive-eval work"
                        (cider-repl-type repl-buffer))))))

       (dup-tried-form-message
        (form)
        (message "nomis/cider/post-interactive-eval DUP TRIED FORM: %s"
                 form))

       (repl-buffer->form-string
        (repl-buffer)
        (let* ((form-symbol
                (case (cider-repl-type repl-buffer)
                  (clj  'nomis/cider/post-interactive-eval/clj-function-name)
                  (cljs 'nomis/cider/post-interactive-eval/cljs-function-name)))
               (form-symbol-value (eval form-symbol)))
          (when form-symbol-value
            (format "(try (let [f (resolve '%s)] (f))
                               (catch #?(:clj Exception :cljs js/Error)
                                   e
                                   (let [message (str \"Have you set up `%s` and the function it refers to (`%s`) properly?  -- Error when evaluating `\" '%s \"` for `nomis/cider/post-interactive-eval`: \" e)]
                                     (throw #?(:clj (Error. message)
                                               :cljs (js/Error. message))))))"
                    form-symbol-value
                    form-symbol
                    form-symbol-value
                    form-symbol-value))))

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
               (let ((tried-form-strings '()))
                 (nomis/memoize
                  ;; Memoize because the callback is called multiple times for
                  ;; each REPL, but we only want to do the post-processing once
                  ;; per REPL.
                  (lambda (repl-buffer)
                    (let ((form-string (repl-buffer->form-string repl-buffer)))
                      (when form-string
                        (if (member form-string tried-form-strings)
                            ;; Sometimes we get here. I've only seen that just
                            ;; after starting Emacs.
                            (dup-tried-form-message form-string)
                          (progn
                            (push form-string tried-form-strings)
                            (run-post-form form-string repl-buffer))))))))))
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
         (do-it (if (not *-nomis/cider/post-interactive-eval/do-advice?*)
                    callback
                  (-> (or callback
                          ;; The following is copied from
                          ;; `cider-request:load-file`.
                          (cider-load-file-handler (current-buffer)))
                      wrap-update-ui)))))
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
