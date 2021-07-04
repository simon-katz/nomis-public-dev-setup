;;;; Init stuff -- CIDER extras --  -*- lexical-binding: t -*-

;;;; ___________________________________________________________________________

(require 'nomis-clojure-test-files)

;;;; ___________________________________________________________________________

(cond
 ((member (nomis/cider-version)
          '("CIDER 0.24.0snapshot"))
  (advice-add
   'cider-repl-handler
   :around
   (lambda (orig-fun buffer)
     (let ((show-prompt t)
           (show-prefix t) ; THIS IS THE CHANGED BIT -- all `show-prefix` stuff
           )
       (nrepl-make-response-handler
        buffer
        (lambda (buffer value)
          (cider-repl-emit-result buffer value show-prefix)
          (setq show-prefix nil))
        (lambda (buffer out)
          (cider-repl-emit-stdout buffer out))
        (lambda (buffer err)
          (cider-repl-emit-stderr buffer err))
        (lambda (buffer)
          (when show-prompt
            (cider-repl-emit-prompt buffer)))
        nrepl-err-handler
        (lambda (buffer value content-type)
          (if-let* ((content-attrs (cadr content-type))
                    (content-type* (car content-type))
                    (handler (cdr (assoc content-type*
                                         cider-repl-content-type-handler-alist))))
              (setq show-prompt (funcall handler content-type buffer value nil t))
            (cider-repl-emit-result buffer value t t)))
        (lambda (buffer warning)
          (cider-repl-emit-stderr buffer warning)))))
   '((name . nomis/cider-avoid-multiple-result-prefixes))))
 ((version<= "0.26.1" (pkg-info-version-info 'cider))
  ;; I think this is now fixed.
  )
 (t
  (message-box
   "You need to fix `nomis/cider-avoid-multiple-result-prefixes` for this version of Cider.")))

;;;; ___________________________________________________________________________

(defun nomis/cider-ns-refresh/-get-log-buffer ()
  ;; Copied from `cider-ns-refresh`:
  (or (get-buffer cider-ns-refresh-log-buffer)
      (cider-make-popup-buffer cider-ns-refresh-log-buffer)))

(defface nomis/cider-ns-refresh/-log-face
  '((t (:foreground "blue3" :italic t)))
  "Face for `nomis/cider-ns-refresh/log`.")

(defun nomis/cider-ns-refresh/log (log-buffer msg)
  (cider-emit-into-popup-buffer log-buffer
                                msg
                                'nomis/cider-ns-refresh/-log-face
                                t))

;;;; ___________________________________________________________________________

;;;; I don't understand why, but the messages displayed in the echo area by
;;;; `cider-ns-refresh--handle-response` often disappear after a short time.
;;;; -- Maybe this only happens when you define one or both of
;;;;    `cider-ns-refresh-before-fn` and `cider-ns-refresh-after-fn`, as you do
;;;;    in `nomis-kafka-clj-examples`.
;;;;    -- Nope, that doesn't seem to be it.
;;;;
;;;; So, I've set `cider-ns-refresh-show-log-buffer`. But that doesn't select
;;;; the log buffer, so we hack things to fix that. And also make sure that we
;;;; disregard any window in another frame that is showing the log buffer.
;;;;
;;;; And a bunch of other cool stuff:
;;;; - Add logging to show the mode of the refresh.
;;;; - Add logging to make boundaries between refreshes clear.
;;;; - Pass refresh-related variables through to the log buffer as
;;;;   buffer-locals.
;;;; - Add commands to jump and delete in the log buffer.
;;;;
;;;; -- jsk 2021-06-28 and later

(defvar nomis/cider-ns-refresh/delay-for-post-message 0.75)

(defvar nomis/cider-ns-refresh/-count 0)

(defvar nomis/cider-ns-refresh/-prefix-for-log-pre-message
  "----------------------------------------\n>>>> Doing cider-ns-refresh")

(defun nomis/cider-ns-refresh/log-pre-message (mode
                                               log-buffer-freshly-created?)
  (s-concat (if log-buffer-freshly-created? "" "\n\n\n")
            (format "%s #%s -- mode = %s"
                    nomis/cider-ns-refresh/-prefix-for-log-pre-message
                    nomis/cider-ns-refresh/-count
                    mode)
            "\n"))

(defun nomis/cider-ns-refresh/log-post-message ()
  (run-at-time
   ;; Without this delay, if the refresh happens very quickly or if no
   ;; refresh is needed, the refresh buffer will pop up and the user
   ;; won't see anything being added to the log buffer. So use
   ;; `run-at-time` so that there is a delay before the post-message
   ;; appears, so that the user sees that something happened.
   nomis/cider-ns-refresh/delay-for-post-message
   nil
   (lambda ()
     (let* ((log-buffer (nomis/cider-ns-refresh/-get-log-buffer))
            (msg (format (s-join
                          "\n"
                          (list
                           "<<<< Done cider-ns-refresh #%s"
                           "Some useful commands:"
                           "  nomis/cider-ns-refresh/backward-section"
                           "  nomis/cider-ns-refresh/delete-to-beginning-of-buffer"
                           "Press \"q\" to exit"))
                         nomis/cider-ns-refresh/-count)))
       (nomis/cider-ns-refresh/log log-buffer msg)))))

(defun nomis/cider-ns-refresh/backward-section ()
  (interactive)
  (condition-case nil
      (search-backward nomis/cider-ns-refresh/-prefix-for-log-pre-message)
    (error
     (nomis/msg/grab-user-attention/high)
     (error "There is no previous section"))))

(defun nomis/cider-ns-refresh/delete-to-beginning-of-buffer ()
  (interactive)
  (let* ((inhibit-read-only t))
    (delete-region 1 (point))))

(cond
 ((member (nomis/cider-version)
          '("CIDER 0.26.1 (Nesebar)"))

  (defvar nomis/cider-ns-refresh/-vars-to-pass-to-log-buffer
    '(nomis/cider-forbid-refresh-all?
      cider-ns-refresh-before-fn
      cider-ns-refresh-after-fn))

  (defun nomis/cider-ns-refresh/-set-vars-in-log-buffer
      (log-buffer-freshly-created?)
    (let* ((vars-vals-to-pass-to-log-buffer
            (mapcar (lambda (var) (list var (symbol-value var)))
                    nomis/cider-ns-refresh/-vars-to-pass-to-log-buffer))
           (log-buffer (nomis/cider-ns-refresh/-get-log-buffer)))
      (with-current-buffer log-buffer
        (dolist (var-val vars-vals-to-pass-to-log-buffer)
          (cl-multiple-value-bind (sym val) var-val
            (let* ((msg (format "Setting %s to %s\n" sym val)))
              (nomis/cider-ns-refresh/log log-buffer msg))
            (make-local-variable sym)
            (set sym val)))
        (when (and log-buffer-freshly-created?
                   (not truncate-lines))
          (let* ((inhibit-message t))
            (toggle-truncate-lines))))))

  (defvar *nomis/cider-ns-refresh/-in-refresh?* nil)

  (advice-add
   'cider-ns-refresh
   :around
   (lambda (orig-fun mode &rest other-args)
     (incf nomis/cider-ns-refresh/-count)
     (let* ((log-buffer-freshly-created?
             (null (get-buffer cider-ns-refresh-log-buffer)))
            (log-buffer (nomis/cider-ns-refresh/-get-log-buffer)))
       (when cider-ns-refresh-show-log-buffer
         ;; Delay this, because we mustn't change the current buffer for
         ;; the code that is running -- people can use a .dir-locals.el
         ;; to define buffer-local variables like
         ;; `cider-ns-refresh-after-fn`.
         ;; (It took a while for me to suss this out when things weren't
         ;; working.)
         (run-at-time 0
                      nil
                      (lambda ()
                        (display-buffer-same-window log-buffer nil))))
       (let* ((msg (nomis/cider-ns-refresh/log-pre-message mode
                                                           log-buffer-freshly-created?)))
         (nomis/cider-ns-refresh/log log-buffer msg))
       (nomis/cider-ns-refresh/-set-vars-in-log-buffer
        log-buffer-freshly-created?))
     (let* ((*nomis/cider-ns-refresh/-in-refresh?* t))
       (apply orig-fun mode other-args)))
   '((name . nomis/cider-ns-refresh/hack)
     (depth . -100)))

  (advice-add
   'cider-popup-buffer-display
   :around
   (lambda (orig-fun buffer &rest other-args)
     (unless *nomis/cider-ns-refresh/-in-refresh?*
       (apply orig-fun buffer other-args)))
   `((name . nomis/cider-ns-refresh/hack)))

  (advice-add
   'cider-ns-refresh--handle-response
   :after
   (lambda (response &rest other-args)
     (nrepl-dbind-response response (status)
       ;; The final call of the `cider-ns-refresh--handle-response` callback
       ;; has a status of `("state")`.
       (when (equal status '("state"))
         (nomis/cider-ns-refresh/log-post-message))))
   '((name . nomis/cider-ns-refresh/hack)))

  ;; (advice-remove 'cider-ns-refresh 'nomis/cider-ns-refresh/hack)
  ;; (advice-remove 'cider-popup-buffer-display 'nomis/cider-ns-refresh/hack)
  ;; (advice-remove 'cider-ns-refresh--handle-response 'nomis/cider-ns-refresh/hack)
  )
 (t
  (message-box
   "You need to fix `nomis/cider-ns-refresh/hack` for this version of CIDER.")))

;;;; ___________________________________________________________________________

(defvar nomis/cider-forbid-refresh-all? nil) ; Use dir-locals to set this when needed.

(advice-add
 'cider-ns-refresh
 :around
 (lambda (orig-fun mode &rest other-args)
   (let* ((log-buffer (nomis/cider-ns-refresh/-get-log-buffer)))
     (when (and nomis/cider-forbid-refresh-all?
                (member mode '(refresh-all 4 clear 16)))
       (let* ((msg "nomis/cider-forbid-refresh-all? is truthy, so I won't refresh-all"))
         (nomis/cider-ns-refresh/log log-buffer (s-concat msg "\n"))
         (nomis/cider-ns-refresh/log-post-message)
         (nomis/msg/grab-user-attention/high)
         (error msg))))
   (apply orig-fun mode args))
 '((name . nomis/maybe-forbid-refresh)
   (depth . 100)))
;; (advice-remove 'cider-ns-refresh 'nomis/maybe-forbid-refresh)

;;;; ___________________________________________________________________________

(pushnew "deftest-msg" cider-test-defining-forms) ; a Wefarm Nabu thing

;;;; ___________________________________________________________________________

(provide 'nomis-cider-extras)
