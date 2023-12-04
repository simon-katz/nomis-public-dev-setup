;;; nomis-window-config.el --- Frame and window configurations -*- lexical-binding: t -*-

(progn) ; this stops `hs-hide-all` from hiding the next comment

;;;; _______________ Requires __________________________________________________

(require 'dash)
(require 's)
(require 'nomis-save-and-read-data)
(require 'treepy)
(require 'cl-format)

;;;; _______________ Customizable variables ____________________________________

(defconst nomis/wc/directory/old-selected-frame
  "~/.emacs-nomis-frame-window-config/old-selected-frame/")

(defconst nomis/wc/directory/single-frame
  "~/.emacs-nomis-frame-window-config/single-frame/")

(defconst nomis/wc/directory/all-frames
  "~/.emacs-nomis-frame-window-config/all-frames/")

(defvar nomis/wc/root-dir-for-searches nil)

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
                       (:restore (first wc-names))))))

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
                        (eq (first form) 'buffer)
                        (not (get-buffer (second form)))))
              form
            (let* ((buffer-name (second form))
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
           (state  (window-state-get (frame-root-window frame) t)))
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
                        (frame-root-window frame)))))

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
    (condition-case err
        (progn
          (-nomis/wc/apply-frame-info-to-frame frame info)
          nil ; no error
          )
      (error
       ;; First do our best to make sure the new frame is entirely
       ;; on screen.
       (cl-multiple-value-bind (monitor-left-px
                                monitor-top-px
                                _monitor-width-px
                                _monitor-height-px)
           (cdr (assoc 'geometry (frame-monitor-attributes frame)))
         (set-frame-parameter frame 'left monitor-left-px)
         (set-frame-parameter frame 'top  monitor-top-px))
       ;; Display error buffer.
       (let* ((buffer (-nomis/wc/make-restore-error-buffer kind wc-name err)))
         (switch-to-buffer buffer))
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
                      (window-state-get nil t)
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
    (let* ((info (first nomis/wc/just-closed-frame-info-list))
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
  (nomis/wc/save-all-frames (s-concat "no-commit-on-exit-"
                                      (nomis/timestamp-yyyy-mm-dd--hh-mm-ss))))

(add-hook 'kill-emacs-hook
          '-nomis/wc/auto-save-all-frames)

;;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;;;; keymap

(prog1 (define-prefix-command 'nomis/wc/keymap)
  (define-key nomis/wc/keymap (kbd "s") 'nomis/wc/old-save-selected-frame)
  (define-key nomis/wc/keymap (kbd "r") 'nomis/wc/old-restore-single-frame-to-selected-frame)
  (define-key nomis/wc/keymap (kbd "/") 'nomis/wc/search-for-file))

;;;; ___________________________________________________________________________

(provide 'nomis-window-config)
