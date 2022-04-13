;;;; Init stuff -- CIDER ns-refresh --  -*- lexical-binding: t -*-

;;;; ___________________________________________________________________________

(defun nomis/cider-ns-refresh/-get-log-buffer ()
  ;; Copied from `cider-ns-refresh`:
  (or (get-buffer cider-ns-refresh-log-buffer)
      (cider-make-popup-buffer cider-ns-refresh-log-buffer)))

(defface nomis/cider-ns-refresh/-log-face
  '((t (:foreground "blue3" :italic t)))
  "Face for `nomis/cider-ns-refresh/log`.")

(defface nomis/cider-ns-refresh/-error-face
  '((t (:foreground "red" :italic t)))
  "Face for `nomis/cider-ns-refresh/log-error`.")

(defun nomis/cider-ns-refresh/log (log-buffer msg)
  (cider-emit-into-popup-buffer log-buffer
                                msg
                                'nomis/cider-ns-refresh/-log-face
                                t))

(defun nomis/cider-ns-refresh/log-error (log-buffer msg)
  (cider-emit-into-popup-buffer log-buffer
                                msg
                                'nomis/cider-ns-refresh/-error-face
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

(defvar nomis/cider-ns-refresh/-count 0)

(defvar nomis/cider-ns-refresh/-prefix-for-log-pre-message
  (s-join
   "\n"
   (list
    "--------------------------------------------------------------------------------"
    ">>>> Doing cider-ns-refresh")))

(defun nomis/cider-ns-refresh/pre-message (mode)
  (format "%s #%s -- mode = %s\n%s\n"
          nomis/cider-ns-refresh/-prefix-for-log-pre-message
          nomis/cider-ns-refresh/-count
          mode
          (nomis/timestamp :date-time-zone)))

(defun nomis/cider-ns-refresh/log-post-message ()
  (let* ((log-buffer (nomis/cider-ns-refresh/-get-log-buffer))
         (msg (format (s-join
                       "\n"
                       (list
                        "<<<< Done cider-ns-refresh #%s"
                        "Some useful commands:"
                        "  nomis/cider-ns-refresh/backward-section              (M-up)"
                        "  nomis/cider-ns-refresh/forward-section               (M-down)"
                        "  nomis/cider-ns-refresh/backward-section-align-top    (M-S-up)"
                        "  nomis/cider-ns-refresh/forward-section-align-top     (M-S-down)"
                        "  nomis/cider-ns-refresh/delete-to-beginning-of-buffer (M-k)"
                        "Press \"q\" to exit\n"))
                      nomis/cider-ns-refresh/-count)))
    (nomis/cider-ns-refresh/log log-buffer msg)))

(defun nomis/cider-ns-refresh/backward-section (align-top?)
  (interactive "P")
  (condition-case nil
      (search-backward nomis/cider-ns-refresh/-prefix-for-log-pre-message)
    (error
     (nomis/msg/grab-user-attention/high)
     (error "There is no previous section")))
  (when align-top?
    (recenter 0)))

(defun nomis/cider-ns-refresh/forward-section (align-top?)
  (interactive "P")
  (let* ((pos (save-excursion
                (condition-case nil
                    (progn
                      (forward-line)
                      (search-forward nomis/cider-ns-refresh/-prefix-for-log-pre-message)
                      (goto-char (match-beginning 0)))
                  (error nil)))))
    (if pos
        (goto-char pos)
      (progn
        (nomis/msg/grab-user-attention/high)
        (error "There is no next section"))))
  (when align-top?
    (recenter 0)))

(defun nomis/cider-ns-refresh/backward-section-align-top ()
  (interactive)
  (nomis/cider-ns-refresh/backward-section t))

(defun nomis/cider-ns-refresh/forward-section-align-top ()
  (interactive)
  (nomis/cider-ns-refresh/forward-section t))

(defun nomis/cider-ns-refresh/delete-to-beginning-of-buffer ()
  (interactive)
  (let* ((inhibit-read-only t))
    (delete-region 1 (point))))

(cond
 ((or (member (nomis/cider-version)
              '("CIDER 0.26.1 (Nesebar)"))
      (member (pkg-info-version-info 'cider)
              '("1.2.0snapshot (package: 20210909.1011)"
                "1.2.0snapshot (package: 20210929.1032)"
                "1.2.0snapshot (package: 20211105.708)")))

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
        (when log-buffer-freshly-created?
          (nomis/cider-ns-refresh/mode t)
          (when (not truncate-lines)
            (let* ((inhibit-message t))
              (toggle-truncate-lines)))))))

  (defvar *nomis/cider-ns-refresh/-in-refresh?* nil)

  (advice-add
   'cider-ns-refresh
   :around
   (lambda (orig-fun mode &rest other-args)
     (unless (cider-repls)
       (nomis/msg/grab-user-attention/high)
       (error "There are no CIDER REPLs associated with this buffer"))
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
       (let* ((b (current-buffer)))
         (switch-to-buffer log-buffer)
         (goto-char (point-max))
         (recenter 0)
         (switch-to-buffer b))
       (let* ((msg (nomis/cider-ns-refresh/pre-message mode)))
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
         (nomis/cider-ns-refresh/log-error log-buffer (s-concat msg "\n"))
         (nomis/cider-ns-refresh/log-post-message)
         (nomis/msg/grab-user-attention/high)
         (error msg))))
   (apply orig-fun mode args))
 '((name . nomis/maybe-forbid-refresh)
   (depth . 100)))
;; (advice-remove 'cider-ns-refresh 'nomis/maybe-forbid-refresh)

;;;; ___________________________________________________________________________

(define-minor-mode nomis/cider-ns-refresh/mode
  "Toggle nomis/cider-ns-refresh/mode."
  :init-value nil
  :lighter " cider-ns-refresh-log"
  :keymap
  `(([M-up]       . nomis/cider-ns-refresh/backward-section)
    ([M-down]     . nomis/cider-ns-refresh/forward-section)
    ([M-S-up]     . nomis/cider-ns-refresh/backward-section-align-top)
    ([M-S-down]   . nomis/cider-ns-refresh/forward-section-align-top)
    (,(kbd "M-k") . nomis/cider-ns-refresh/delete-to-beginning-of-buffer)))

;;;; ___________________________________________________________________________

(cond
 ((member (pkg-info-version-info 'cider)
          '("1.2.0snapshot (package: 20211105.708)"))

  (defvar *nomis/cider-ns-refresh/-in-handle-response?* nil)

  (advice-add
   'cider-ns-refresh--handle-response
   :around
   (lambda (orig-fun &rest args)
     (let* ((*nomis/cider-ns-refresh/-in-handle-response?* t))
       (apply orig-fun args)))
   '((name . nomis/cider-ns-refresh/multiple-lines)))

  (advice-add
   'format
   :around
   (lambda (orig-fun string &rest objects)
     (if (not (and *nomis/cider-ns-refresh/-in-handle-response?*
                   (equal string "Reloading %s\n")))
         (apply orig-fun string objects)
       (let* ((*nomis/cider-ns-refresh/-in-handle-response?* nil))
         (apply #'s-concat (-map (lambda (x) (format string x))
                                 (first objects))))))
   '((name . nomis/cider-ns-refresh/multiple-lines)))

  ;; (advice-remove 'cider-ns-refresh--handle-response 'nomis/cider-ns-refresh/multiple-lines)
  ;; (advice-remove 'format 'nomis/cider-ns-refresh/multiple-lines)
  )
 (t
  (message-box
   "You need to fix `nomis/cider-ns-refresh/multiple-lines` for this version of CIDER.")))

;;;; ___________________________________________________________________________

(provide 'nomis-cider-ns-refresh)
