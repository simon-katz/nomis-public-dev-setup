;;; nomis-window-config.el --- Frame and window configurations -*- lexical-binding: t -*-

(progn) ; this stops `hs-hide-all` from hiding the next comment

;;;; _______________ Requires __________________________________________________

(require 'dash)
(require 's)
(require 'nomis-save-and-read-data)
(require 'treepy)
(require 'cl-format)
(require 'nomis-msg)
(require 'homeless-non-lexical)

;;;; _______________ Customizable variables ____________________________________

(defgroup nomis/wc/group nil
  "Frame and window configurations."
  :group 'tools)

(defconst nomis/wc/directory/old-selected-frame
  "~/.emacs-nomis-frame-window-config/old-selected-frame/")

(defconst nomis/wc/directory/single-frame
  "~/.emacs-nomis-frame-window-config/single-frame/")

(defconst nomis/wc/directory/all-frames
  "~/.emacs-nomis-frame-window-config/all-frames/")

(defvar nomis/wc/root-dir-for-searches nil)

(defcustom nomis/wc/point-at-end/always-modes '(vterm-mode)
  "Major modes for which point is always restored to end of buffer on
window config restore."
  :type '(repeat symbol)
  :group 'nomis/wc/group)

(defcustom nomis/wc/point-at-end/modes '(cider-repl-mode
                                         messages-buffer-mode)
  "Major modes for which point is conditionally restored to end of buffer on
window config restore -- only if point was at end of buffer when the config
was saved."
  :type '(repeat symbol)
  :group 'nomis/wc/group)

(defcustom nomis/wc/point-at-end/buffer-name-regexps '()
  "Buffer name regexps for which point is conditionally restored to end of
buffer on window config restore -- only if point was at end of buffer when
the config was saved."
  :type '(repeat regexp)
  :group 'nomis/wc/group)

;;;; _______________ Private things ____________________________________________

(defconst -nomis/wc/old-file-suffix
  ".window-config")

(defconst -nomis/wc/single-frame-file-suffix
  ".frame-config")

(defconst -nomis/wc/all-frames-file-suffix
  ".all-frames-config")

(defun -nomis/wc/wc-name->filename (wc-name directory file-suffix)
  (concat directory wc-name file-suffix))

(defun -nomis/wc/interactive-wc-name-stuff (save-or-restore
                                            directory
                                            file-suffix)
  (let* ((wc-names (when (file-directory-p directory)
                     (->> (directory-files directory)
                          (-remove (lambda (filename)
                                     (member filename
                                             (list "." ".."))))
                          (-filter (lambda (filename)
                                     (s-ends-with? file-suffix
                                                   filename)))
                          (-map (lambda (filename)
                                  (s-replace file-suffix
                                             ""
                                             filename)))))))
    (when (and (null wc-names)
               (eq save-or-restore :restore))
      (error "No saved configurations"))
    (completing-read "Name: "
                     wc-names
                     nil
                     (cl-ecase save-or-restore
                       (:save nil)
                       (:restore t))
                     nil
                     'nomis/wc/wc-name-history
                     (cl-ecase save-or-restore
                       (:save "")
                       (:restore (cl-first wc-names))))))

(defconst -nomis/wc/no-such-buffer-prefix "*NO-SUCH-BUFFER--")
(defconst -nomis/wc/no-such-buffer-suffix "*")

(defun -nomis/wc/buffer-name->proxy-buffer-name (buffer-name)
  (concat -nomis/wc/no-such-buffer-prefix
          buffer-name
          -nomis/wc/no-such-buffer-suffix))

(defun -nomis/wc/proxy-buffer-name->filename (proxy-buffer-name)
  (let* ((prefix -nomis/wc/no-such-buffer-prefix)
         (suffix -nomis/wc/no-such-buffer-suffix))
    (if (or (not (s-starts-with? prefix proxy-buffer-name))
            (not (s-ends-with? suffix proxy-buffer-name)))
        (progn
          (error "This buffer is not a NO-SUCH-BUFFER buffer")
          (nomis/msg/beep))
      (->> proxy-buffer-name
           (replace-regexp-in-string (concat "^" (regexp-quote prefix))
                                     "")
           (replace-regexp-in-string (concat (regexp-quote suffix) "$")
                                     "")
           file-name-nondirectory ; because `find-name-dired` needs just a filename
           ))))

(defun -nomis/wc/get-or-create-buffer-for-no-such-buffer (buffer-name)
  (let* ((proxy-buffer-name (-nomis/wc/buffer-name->proxy-buffer-name
                             buffer-name)))
    (or (get-buffer proxy-buffer-name)
        (let* ((buffer (generate-new-buffer proxy-buffer-name)))
          (with-current-buffer buffer
            (insert (format "NO SUCH BUFFER: %s\n"
                            buffer-name))
            (insert (format "To search for the file, use the command `nomis/wc/search-for-file`.\n"))
            (read-only-mode 1))
          buffer))))

(defun -nomis/wc/window-state/replace-unknown-buffers* (window-state)
  (->> window-state
       (treepy-prewalk
        (lambda (form)
          (if (not (and (listp form)
                        (eq (cl-first form) 'buffer)
                        (not (get-buffer (cl-second form)))))
              form
            (let* ((buffer-name (cl-second form))
                   (proxy-buffer-name (-nomis/wc/buffer-name->proxy-buffer-name
                                       buffer-name)))
              (-nomis/wc/get-or-create-buffer-for-no-such-buffer buffer-name)
              (-replace-at 1 proxy-buffer-name form)))))))

(defun -nomis/wc/window-state/replace-unknown-buffers (window-state)
  (condition-case err
      (-nomis/wc/window-state/replace-unknown-buffers* window-state)
    (error (message "WTF replace-unknown-buffers error: %s" err)
           (message-box "WTF replace-unknown-buffers error: %s" err)
           window-state)))

(defun -nomis/wc/point-at-end/buffer-matches-filter? (buffer)
  (with-current-buffer buffer
    (or (memq major-mode nomis/wc/point-at-end/modes)
        (-any? (lambda (regexp)
                 (string-match-p regexp (buffer-name buffer)))
               nomis/wc/point-at-end/buffer-name-regexps))))

(defun -nomis/wc/window-state/add-point-at-end (window-state)
  (->> window-state
       (treepy-prewalk
        (lambda (form)
          (if (not (and (listp form) (eq (car form) 'leaf)))
              form
            (let* ((buffer-entry  (assq 'buffer (cdr form)))
                   (buffer-name   (when buffer-entry (nth 1 buffer-entry)))
                   (buffer        (when buffer-name (get-buffer buffer-name)))
                   (saved-point   (when buffer-entry
                                    (cdr (assq 'point (cddr buffer-entry)))))
                   (at-end?       (when (and buffer saved-point)
                                    (with-current-buffer buffer
                                      (= saved-point (point-max))))))
              (if (not at-end?)
                  form
                (let* ((rest            (cdr form))
                       (params-entry    (assq 'parameters rest))
                       (existing-params (cdr params-entry))
                       (new-params-entry ;
                        (cons 'parameters
                              (cons '(nomis/point-at-end . t)
                                    existing-params)))
                       (new-rest        (cons new-params-entry
                                              (-remove (lambda (e)
                                                         (and (consp e)
                                                              (eq (car e)
                                                                  'parameters)))
                                                       rest))))
                  (cons 'leaf new-rest)))))))))

(defun -nomis/wc/window-state/restore-point-at-end (frame)
  (walk-windows
   (lambda (win)
     (let* ((buffer (window-buffer win)))
       (when (or (with-current-buffer buffer
                   (memq major-mode nomis/wc/point-at-end/always-modes))
                 (and (window-parameter win 'nomis/point-at-end)
                      (-nomis/wc/point-at-end/buffer-matches-filter? buffer)))
         (with-selected-window win
           (goto-char (point-max))))))
   nil
   frame))

(defun -nomis/wc/frame->frame-info (frame)
  ()
  (cl-multiple-value-bind (monitor-left-px
                           monitor-top-px
                           monitor-width-px
                           monitor-height-px)
      (cdr (assoc 'geometry (frame-monitor-attributes frame)))
    (let* ((info   (make-hash-table))
           (left-px   (- (frame-parameter frame 'left)
                         monitor-left-px))
           (top-px    (- (frame-parameter frame 'top)
                         monitor-top-px))
           (width-px  (* (frame-parameter frame 'width) (frame-char-width frame)))
           (height-px (* (frame-parameter frame 'height) (frame-char-height frame)))
           (state  (-> (window-state-get (frame-root-window frame) t)
                       -nomis/wc/window-state/add-point-at-end)))
      (puthash :monitor-width-px  monitor-width-px  info)
      (puthash :monitor-height-px monitor-height-px info)
      (puthash :left-px           left-px   info)
      (puthash :top-px            top-px    info)
      (puthash :width-px          width-px  info)
      (puthash :height-px         height-px info)
      (puthash :state          state  info)
      info)))

(defun -nomis/wc/apply-frame-info-to-frame (frame info)
  (cl-multiple-value-bind (monitor-left-px
                           monitor-top-px
                           monitor-width-px
                           monitor-height-px)
      (cdr (assoc 'geometry (frame-monitor-attributes frame)))
    (let* ((saved-monitor-width-px  (or (gethash :monitor-width-px  info) monitor-width-px))
           (saved-monitor-height-px (or (gethash :monitor-height-px info) monitor-height-px))
           (left-px   (gethash :left-px   info))
           (top-px    (gethash :top-px    info))
           (width-px  (gethash :width-px  info))
           (height-px (gethash :height-px info))
           (state  (gethash :state  info))
           (x-multiplier (if (not (= saved-monitor-width-px monitor-width-px))
                             (/ monitor-width-px 1.0  saved-monitor-width-px)
                           1))
           (y-multiplier (if (not (= saved-monitor-height-px monitor-height-px))
                             (/ monitor-height-px 1.0 saved-monitor-height-px)
                           1)))
      (set-frame-parameter frame 'left   (+ (floor (* x-multiplier left-px))
                                            monitor-left-px))
      (set-frame-parameter frame 'top    (+ (floor (* y-multiplier top-px))
                                            monitor-top-px))
      (set-frame-parameter frame 'width  (floor (/ (* x-multiplier width-px)
                                                   1.0
                                                   (frame-char-width frame))))
      (set-frame-parameter frame 'height (floor (/ (* y-multiplier height-px)
                                                   1.0
                                                   (frame-char-height frame))))
      (window-state-put (-nomis/wc/window-state/replace-unknown-buffers state)
                        (frame-root-window frame))
      (-nomis/wc/window-state/restore-point-at-end frame))))

(defun -nomis/wc/make-restore-error-buffer (kind wc-name err)
  (let* ((title (format "Failed to restore %s %s" kind wc-name))
         (text (format "%s\n\n%s" title err))
         (buffer (generate-new-buffer title)))
    (with-current-buffer buffer
      (insert text)
      (read-only-mode 1))
    buffer))

(defun -nomis/wc/window-state/make-frame-using-frame-info (kind wc-name info)
  ;; Returns nil for success, otherwise an error.
  (let* ((frame (make-frame-on-current-monitor)))
    ;; Switch to "*GNU Emacs*" buffer, because some error messages
    ;; mention the current buffer, and this buffer name won't lead to
    ;; confusing error messages.
    (switch-to-buffer (get-buffer-create "*Window-Config-Restore-Temp*"))
    (condition-case err
        (progn
          (-nomis/wc/apply-frame-info-to-frame frame info)
          nil ; no error
          )
      (error
       ;; First make sure the new frame is entirely on screen.
       (cl-multiple-value-bind (monitor-left-px
                                monitor-top-px
                                monitor-width-px
                                monitor-height-px)
           (cdr (assoc 'geometry (frame-monitor-attributes frame)))
         (set-frame-parameter frame 'left monitor-left-px)
         (set-frame-parameter frame 'top  monitor-top-px)
         (set-frame-parameter frame 'width (floor (/ monitor-width-px
                                                     2.0
                                                     (frame-char-width frame))))
         (set-frame-parameter frame 'height (floor (/ monitor-height-px
                                                      2.0
                                                      (frame-char-height frame)))))
       ;; Display error buffer.
       (switch-to-buffer
        (-nomis/wc/make-restore-error-buffer kind wc-name err))
       ;; Now deal with the error.
       (message "Failed to restore frame: %s" err)
       ;; The following often causes a crash, so comment out.
       ;; (when (nomis/y-or-n-p-with-quit->nil
       ;;        (format "Failed to restore frame. Delete the new frame? (Got: %s)"
       ;;                err))
       ;;   (let* ((*nomis/wc/no-note-deleted-frames?* t))
       ;;     (delete-frame frame)))
       err))))

;;;; _______________ Public functions etc ______________________________________

;;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;;;; Old approach -- save current frame (windows only, not size and
;;;; position), and restore to current frame

(defun nomis/wc/old-save-selected-frame (wc-name)
  (interactive (list (-nomis/wc/interactive-wc-name-stuff
                      :save
                      nomis/wc/directory/old-selected-frame
                      -nomis/wc/old-file-suffix)))
  (nomis/save-to-file (-nomis/wc/wc-name->filename
                       wc-name
                       nomis/wc/directory/old-selected-frame
                       -nomis/wc/old-file-suffix)
                      (-> (window-state-get nil t)
                          -nomis/wc/window-state/add-point-at-end)
                      :pretty? t)
  (message "Saved window config: %s" wc-name))

(defun nomis/wc/old-restore-single-frame-to-selected-frame (wc-name)
  (interactive (list (-nomis/wc/interactive-wc-name-stuff
                      :restore
                      nomis/wc/directory/old-selected-frame
                      -nomis/wc/old-file-suffix)))
  (let* ((filename (-nomis/wc/wc-name->filename
                    wc-name
                    nomis/wc/directory/old-selected-frame
                    -nomis/wc/old-file-suffix))
         (window-state (nomis/read-from-file filename))
         (hacked-window-state (-nomis/wc/window-state/replace-unknown-buffers
                               window-state)))
    (window-state-put hacked-window-state
                      (frame-root-window))
    (-nomis/wc/window-state/restore-point-at-end (selected-frame))
    (message "Restored window config: %s"
             wc-name)))

;;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;;;; Save current frame (windows, size and position), and restore to
;;;; a new frame

(defun nomis/wc/save-selected-frame (wc-name)
  (interactive (list (-nomis/wc/interactive-wc-name-stuff
                      :save
                      nomis/wc/directory/single-frame
                      -nomis/wc/single-frame-file-suffix)))
  (nomis/save-to-file (-nomis/wc/wc-name->filename
                       wc-name
                       nomis/wc/directory/single-frame
                       -nomis/wc/single-frame-file-suffix)
                      (-nomis/wc/frame->frame-info (selected-frame))
                      :pretty? t)
  (message "Saved selected frame config: %s" wc-name))

(defun nomis/wc/restore-single-frame (wc-name)
  (interactive (list (-nomis/wc/interactive-wc-name-stuff
                      :restore
                      nomis/wc/directory/single-frame
                      -nomis/wc/single-frame-file-suffix)))
  (let* ((filename (-nomis/wc/wc-name->filename
                    wc-name
                    nomis/wc/directory/single-frame
                    -nomis/wc/single-frame-file-suffix))
         (info (nomis/read-from-file filename))
         (err (-nomis/wc/window-state/make-frame-using-frame-info
               'single
               wc-name
               info)))
    (message (if err
                 "Failed to restore single frame config: %s"
               "Restored single frame config: %s")
             wc-name)))

;;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;;;; Save current frame (windows, size and position), and restore to
;;;; a new frame

(defun nomis/wc/save-all-frames (wc-name)
  (interactive (list (-nomis/wc/interactive-wc-name-stuff
                      :save
                      nomis/wc/directory/all-frames
                      -nomis/wc/all-frames-file-suffix)))
  (nomis/save-to-file (-nomis/wc/wc-name->filename
                       wc-name
                       nomis/wc/directory/all-frames
                       -nomis/wc/all-frames-file-suffix)
                      (-map #'-nomis/wc/frame->frame-info
                            (frame-list))
                      :pretty? t)
  (message "Saved all-frames config: %s" wc-name))

(defun nomis/wc/restore-multiple-frames (wc-name)
  (interactive (list (-nomis/wc/interactive-wc-name-stuff
                      :restore
                      nomis/wc/directory/all-frames
                      -nomis/wc/all-frames-file-suffix)))
  (let* ((frames-to-delete (when (y-or-n-p "Delete existing frames?")
                             (frame-list)))
         (filename (-nomis/wc/wc-name->filename
                    wc-name
                    nomis/wc/directory/all-frames
                    -nomis/wc/all-frames-file-suffix))
         (infos (nomis/read-from-file filename))
         (errors? nil))
    (dolist (info infos)
      (let* ((err (-nomis/wc/window-state/make-frame-using-frame-info
                   'multiple
                   wc-name
                   info)))
        (when err (setq errors? t))))
    (dolist (frame frames-to-delete)
      (delete-frame frame))
    (message "Restored all-frames config: %s%s"
             wc-name
             (if errors? " with errors" ""))))

;;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;;;; Restore just-closed frame

(defvar nomis/wc/just-closed-frame-info-list '())

(defvar *nomis/wc/no-note-deleted-frames?* nil)

(add-hook 'delete-frame-functions 'nomis/wc/note-deleted-frame)

(defun nomis/wc/note-deleted-frame (frame)
  (unless *nomis/wc/no-note-deleted-frames?*
    (push (-nomis/wc/frame->frame-info frame)
          nomis/wc/just-closed-frame-info-list)))

(defun nomis/wc/restore-just-deleted-frame ()
  (interactive)
  (if (null nomis/wc/just-closed-frame-info-list)
      (user-error "There is no deleted frame to restore")
    (let* ((info (cl-first nomis/wc/just-closed-frame-info-list))
           (err (-nomis/wc/window-state/make-frame-using-frame-info
                 "just-deleted-frame"
                 "(just-deleted-frame)"
                 info)))
      (unless err
        ;; We don't do this pop if we fail to restore state (/eg/
        ;; because frame is too small).
        (pop nomis/wc/just-closed-frame-info-list)))))

;;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;;;; nomis/wc/search-for-file

(defun nomis/wc/search-for-file ()
  (interactive)
  (let* ((filename (-nomis/wc/proxy-buffer-name->filename (buffer-name)))
         (root-directory (read-directory-name
                          (format "Search for %s\nRoot of search: "
                                  filename)
                          nomis/wc/root-dir-for-searches
                          nil
                          t)))
    (find-name-dired root-directory
                     filename)))

;;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;;;; Auto-save on exit

(defun -nomis/wc/auto-save-all-frames ()
  (nomis/wc/save-all-frames (s-concat "_no-commit_on-exit-"
                                      (nomis/timestamp-yyyy-mm-dd--hh-mm-ss))))

(add-hook 'kill-emacs-hook
          '-nomis/wc/auto-save-all-frames)

;;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;;;; keymap

(defvar nomis/wc/keymap)
(prog1 (define-prefix-command 'nomis/wc/keymap)
  (define-key nomis/wc/keymap (kbd "s") 'nomis/wc/old-save-selected-frame)
  (define-key nomis/wc/keymap (kbd "r") 'nomis/wc/old-restore-single-frame-to-selected-frame)
  (define-key nomis/wc/keymap (kbd "/") 'nomis/wc/search-for-file)
  (define-key nomis/wc/keymap (kbd "t") 'nomis/wc/restore-just-deleted-frame))

(define-key global-map (kbd "M-T") 'nomis/wc/restore-just-deleted-frame)

;;;; ___________________________________________________________________________

(provide 'nomis-window-config)
