;;; nomis-find-file-noselect-without-confusing-messages.el -*- lexical-binding:t

(defconst nomis/find-file-noselect--no-prompt-if-buffer-exists? t)

(cond
 ((member emacs-version
          '("28.2"))
  ;; The original is in `files`.
  (defun find-file-noselect (filename &optional nowarn rawfile wildcards)
    "Read file FILENAME into a buffer and return the buffer.
If a buffer exists visiting FILENAME, return that one, but
verify that the file has not changed since visited or saved.
The buffer is not selected, just returned to the caller.
Optional second arg NOWARN non-nil means suppress any warning messages.
Optional third arg RAWFILE non-nil means the file is read literally.
Optional fourth arg WILDCARDS non-nil means do wildcard processing
and visit all the matching files.  When wildcards are actually
used and expanded, return a list of buffers that are visiting
the various files."
    (setq filename
          (abbreviate-file-name
           (expand-file-name filename)))
    (if (file-directory-p filename)
        (or (and find-file-run-dired
                 (run-hook-with-args-until-success
                  'find-directory-functions
                  (if find-file-visit-truename
                      (abbreviate-file-name (file-truename filename))
                    filename)))
            (error "%s is a directory" filename))
      (if (and wildcards
               find-file-wildcards
               (not (file-name-quoted-p filename))
               (string-match "[[*?]" filename))
          (let ((files (condition-case nil
                           (file-expand-wildcards filename t)
                         (error (list filename))))
                (find-file-wildcards nil))
            (if (null files)
                (find-file-noselect filename)
              (mapcar #'find-file-noselect files)))
        (let* ((buf (get-file-buffer filename))
               (truename (abbreviate-file-name (file-truename filename)))
               (attributes (file-attributes truename))
               (number (nthcdr 10 attributes))
               ;; Find any buffer for a file that has same truename.
               (other (and (not buf) (find-buffer-visiting filename))))
          ;; Let user know if there is a buffer with the same truename.
          (if other
              (progn
                (or nowarn
                    find-file-suppress-same-file-warnings
                    (string-equal filename (buffer-file-name other))
                    (files--message "%s and %s are the same file"
                                    filename (buffer-file-name other)))
                ;; Optionally also find that buffer.
                (if (or find-file-existing-other-name find-file-visit-truename)
                    (setq buf other))))
          ;; Check to see if the file looks uncommonly large.
          (when (not (or buf nowarn))
            (when (eq (abort-if-file-too-large
                       (file-attribute-size attributes) "open" filename
                       (not rawfile))
                      'raw)
              (setf rawfile t))
            (warn-maybe-out-of-memory (file-attribute-size attributes)))
          (if buf
              ;; We are using an existing buffer.
              (let (nonexistent)
                (or nowarn
                    (verify-visited-file-modtime buf)
                    (cond ((not (file-exists-p filename))
                           (setq nonexistent t)
                           (message "File %s no longer exists!" filename))
                          ;; Certain files should be reverted automatically
                          ;; if they have changed on disk and not in the buffer.
                          ((and (not (buffer-modified-p buf))
                                (let ((tail revert-without-query)
                                      (found nil))
                                  (while tail
                                    (if (string-match (car tail) filename)
                                        (setq found t))
                                    (setq tail (cdr tail)))
                                  found))
                           (with-current-buffer buf
                             (message "Reverting file %s..." filename)
                             (revert-buffer t t)
                             (message "Reverting file %s...done" filename)))
                          ((not query-about-changed-file)
                           (message
                            (substitute-command-keys
                             "File %s changed on disk.  \\[revert-buffer-quick] to load new contents%s")
                            (file-name-nondirectory filename)
                            (if (buffer-modified-p buf)
                                " and discard your edits"
                              "")))
                          ((yes-or-no-p
                            (if (string= (file-name-nondirectory filename)
                                         (buffer-name buf))
                                (format
                                 (if (buffer-modified-p buf)
                                     "File %s changed on disk.  Discard your edits? "
                                   "File %s changed on disk.  Reread from disk? ")
                                 (file-name-nondirectory filename))
                              (format
                               (if (buffer-modified-p buf)
                                   "File %s changed on disk.  Discard your edits in %s? "
                                 "File %s changed on disk.  Reread from disk into %s? ")
                               (file-name-nondirectory filename)
                               (buffer-name buf))))
                           (with-current-buffer buf
                             (revert-buffer t t)))))
                (with-current-buffer buf

                  ;; Check if a formerly read-only file has become
                  ;; writable and vice versa, but if the buffer agrees
                  ;; with the new state of the file, that is ok too.
                  (let ((read-only (not (file-writable-p buffer-file-name))))
                    (unless (or nonexistent
                                (eq read-only buffer-file-read-only)
                                (eq read-only buffer-read-only))
                      (when (or nowarn
                                (let* ((new-status
                                        (if read-only "read-only" "writable"))
                                       (question
                                        (format "File %s is %s on disk.  Make buffer %s, too? "
                                                buffer-file-name
                                                new-status new-status)))
                                  (y-or-n-p question)))
                        (setq buffer-read-only read-only)))
                    (setq buffer-file-read-only read-only))

                  (unless (or (eq (null rawfile) (null find-file-literally))
                              nonexistent
                              ;; It is confusing to ask whether to visit
                              ;; non-literally if they have the file in
                              ;; hexl-mode or image-mode.
                              (memq major-mode '(hexl-mode image-mode)))
                    (if (buffer-modified-p)
                        (if (let ((help-form
                                   (format-message
                                    (if rawfile "\
The file %s is already visited normally,
and you have edited the buffer.  Now you have asked to visit it literally,
meaning no coding system handling, format conversion, or local variables.
Emacs can visit a file in only one way at a time."
                                      "\
The file %s is already visited literally,
meaning no coding system handling, format conversion, or local variables.
You have edited the buffer.  Now you have asked to visit the file normally,
but Emacs can visit a file in only one way at a time.")
                                    (file-name-nondirectory filename))))
                              (y-or-n-p
                               (if rawfile "\
Do you want to save the file, and visit it literally instead? " "\
Do you want to save the file, and visit it normally instead? ")))
                            (progn
                              (save-buffer)
                              (find-file-noselect-1 buf filename nowarn
                                                    rawfile truename number))
                          (if (y-or-n-p
                               (if rawfile "\
Do you want to discard your changes, and visit the file literally now? " "\
Do you want to discard your changes, and visit the file normally now? "))
                              (find-file-noselect-1 buf filename nowarn
                                                    rawfile truename number)
                            (error (if rawfile "File already visited non-literally"
                                     "File already visited literally"))))
                      (if nomis/find-file-noselect--no-prompt-if-buffer-exists?
                          (pop-to-buffer buf)
                        (if (let ((help-form
                                   (format-message
                                    (if rawfile "\
The file %s is already visited normally.
You have asked to visit it literally,
meaning no coding system decoding, format conversion, or local variables.
But Emacs can visit a file in only one way at a time."
                                      "\
The file %s is already visited literally,
meaning no coding system decoding, format conversion, or local variables.
You have asked to visit it normally,
but Emacs can visit a file in only one way at a time.")
                                    (file-name-nondirectory filename))))
                              (y-or-n-p
                               (if rawfile "\
Do you want to revisit the file literally now? " "\
Do you want to revisit the file normally now? ")))
                            (find-file-noselect-1 buf filename nowarn
                                                  rawfile truename number)
                          (error (if rawfile "File already visited non-literally"
                                   "File already visited literally")))))))
                ;; Return the buffer we are using.
                buf)
            ;; Create a new buffer.
            (setq buf (create-file-buffer filename))
            ;; find-file-noselect-1 may use a different buffer.
            (find-file-noselect-1 buf filename nowarn
                                  rawfile truename number)))))))

 ((member emacs-version
          '("29.4"
            "30.1"))
  ;; The original is in `files`.
  (defun find-file-noselect (filename &optional nowarn rawfile wildcards)
    "Read file FILENAME into a buffer and return the buffer.
If a buffer exists visiting FILENAME, return that one, but
verify that the file has not changed since visited or saved.
The buffer is not selected, just returned to the caller.
Optional second arg NOWARN non-nil means suppress any warning messages.
Optional third arg RAWFILE non-nil means the file is read literally.
Optional fourth arg WILDCARDS non-nil means do wildcard processing
and visit all the matching files.  When wildcards are actually
used and expanded, return a list of buffers that are visiting
the various files."
    (setq filename
          (abbreviate-file-name
           (expand-file-name filename)))
    (if (file-directory-p filename)
        (or (and find-file-run-dired
                 (run-hook-with-args-until-success
                  'find-directory-functions
                  (if find-file-visit-truename
                      (abbreviate-file-name (file-truename filename))
                    filename)))
            (error "%s is a directory" filename))
      (if (and wildcards
               find-file-wildcards
               (not (file-name-quoted-p filename))
               (string-match "[[*?]" filename))
          (let ((files (condition-case nil
                           (file-expand-wildcards filename t)
                         (error (list filename))))
                (find-file-wildcards nil))
            (if (null files)
                (find-file-noselect filename)
              (mapcar #'find-file-noselect files)))
        (let* ((buf (get-file-buffer filename))
               (truename (abbreviate-file-name (file-truename filename)))
               (attributes (file-attributes truename))
               (number (file-attribute-file-identifier attributes))
               ;; Find any buffer for a file that has same truename.
               (other (and (not buf)
                           (find-buffer-visiting
                            filename
                            ;; We want to filter out buffers that we've
                            ;; visited via symlinks and the like, where
                            ;; the symlink no longer exists.
                            (lambda (buffer)
                              (let ((file (buffer-local-value
                                           'buffer-file-name buffer)))
                                (and file (file-exists-p file))))))))
          ;; Let user know if there is a buffer with the same truename.
          (when other
            (or nowarn
                find-file-suppress-same-file-warnings
                (string-equal filename (buffer-file-name other))
                (files--message "%s and %s are the same file"
                                filename (buffer-file-name other)))
            ;; Optionally also find that buffer.
            (if (or find-file-existing-other-name find-file-visit-truename)
                (setq buf other)))
          ;; Check to see if the file looks uncommonly large.
          (when (not (or buf nowarn))
            (when (eq (abort-if-file-too-large
                       (file-attribute-size attributes) "open" filename
                       (not rawfile))
                      'raw)
              (setf rawfile t))
            (warn-maybe-out-of-memory (file-attribute-size attributes)))
          (if buf
              ;; We are using an existing buffer.
              (let (nonexistent)
                (or nowarn
                    (verify-visited-file-modtime buf)
                    (cond ((not (file-exists-p filename))
                           (setq nonexistent t)
                           (message "File %s no longer exists!" filename))
                          ;; Certain files should be reverted automatically
                          ;; if they have changed on disk and not in the buffer.
                          ((and (not (buffer-modified-p buf))
                                (let ((tail revert-without-query)
                                      (found nil))
                                  (while tail
                                    (if (string-match (car tail) filename)
                                        (setq found t))
                                    (setq tail (cdr tail)))
                                  found))
                           (with-current-buffer buf
                             (message "Reverting file %s..." filename)
                             (revert-buffer t t)
                             (message "Reverting file %s...done" filename)))
                          ((not query-about-changed-file)
                           (message
                            (substitute-command-keys
                             "File %s changed on disk.  \\[revert-buffer-quick] to load new contents%s")
                            (file-name-nondirectory filename)
                            (if (buffer-modified-p buf)
                                " and discard your edits"
                              "")))
                          ((yes-or-no-p
                            (if (string= (file-name-nondirectory filename)
                                         (buffer-name buf))
                                (format
                                 (if (buffer-modified-p buf)
                                     "File %s changed on disk.  Discard your edits? "
                                   "File %s changed on disk.  Reread from disk? ")
                                 (file-name-nondirectory filename))
                              (format
                               (if (buffer-modified-p buf)
                                   "File %s changed on disk.  Discard your edits in %s? "
                                 "File %s changed on disk.  Reread from disk into %s? ")
                               (file-name-nondirectory filename)
                               (buffer-name buf))))
                           (with-current-buffer buf
                             (revert-buffer t t)))))
                (with-current-buffer buf

                  ;; Check if a formerly read-only file has become
                  ;; writable and vice versa, but if the buffer agrees
                  ;; with the new state of the file, that is ok too.
                  (let ((read-only (not (file-writable-p buffer-file-name))))
                    (unless (or nonexistent
                                (eq read-only buffer-file-read-only)
                                (eq read-only buffer-read-only))
                      (when (or nowarn
                                (let* ((new-status
                                        (if read-only "read-only" "writable"))
                                       (question
                                        (format "File %s is %s on disk.  Make buffer %s, too? "
                                                buffer-file-name
                                                new-status new-status)))
                                  (y-or-n-p question)))
                        (setq buffer-read-only read-only)))
                    (setq buffer-file-read-only read-only))

                  (unless (or (eq (null rawfile) (null find-file-literally))
                              nonexistent
                              ;; It is confusing to ask whether to visit
                              ;; non-literally if they have the file in
                              ;; hexl-mode or image-mode.
                              (memq major-mode '(hexl-mode image-mode)))
                    (if (buffer-modified-p)
                        (if (let ((help-form
                                   (format-message
                                    (if rawfile "\
The file %s is already visited normally,
and you have edited the buffer.  Now you have asked to visit it literally,
meaning no coding system handling, format conversion, or local variables.
Emacs can visit a file in only one way at a time."
                                      "\
The file %s is already visited literally,
meaning no coding system handling, format conversion, or local variables.
You have edited the buffer.  Now you have asked to visit the file normally,
but Emacs can visit a file in only one way at a time.")
                                    (file-name-nondirectory filename))))
                              (y-or-n-p
                               (if rawfile "\
Do you want to save the file, and visit it literally instead? " "\
Do you want to save the file, and visit it normally instead? ")))
                            (progn
                              (save-buffer)
                              (find-file-noselect-1 buf filename nowarn
                                                    rawfile truename number))
                          (if (y-or-n-p
                               (if rawfile "\
Do you want to discard your changes, and visit the file literally now? " "\
Do you want to discard your changes, and visit the file normally now? "))
                              (find-file-noselect-1 buf filename nowarn
                                                    rawfile truename number)
                            (error (if rawfile "File already visited non-literally"
                                     "File already visited literally"))))
                      (if nomis/find-file-noselect--no-prompt-if-buffer-exists?
                          (pop-to-buffer buf)
                        (if (let ((help-form
                                   (format-message
                                    (if rawfile "\
The file %s is already visited normally.
You have asked to visit it literally,
meaning no coding system decoding, format conversion, or local variables.
But Emacs can visit a file in only one way at a time."
                                      "\
The file %s is already visited literally,
meaning no coding system decoding, format conversion, or local variables.
You have asked to visit it normally,
but Emacs can visit a file in only one way at a time.")
                                    (file-name-nondirectory filename))))
                              (y-or-n-p
                               (if rawfile "\
Do you want to revisit the file literally now? " "\
Do you want to revisit the file normally now? ")))
                            (find-file-noselect-1 buf filename nowarn
                                                  rawfile truename number)
                          (error (if rawfile "File already visited non-literally"
                                   "File already visited literally")))))))
                ;; Return the buffer we are using.
                buf)
            ;; Create a new buffer.
            (setq buf (create-file-buffer filename))
            ;; find-file-noselect-1 may use a different buffer.
            (find-file-noselect-1 buf filename nowarn
                                  rawfile truename number)))))))

 ((member emacs-version
          '("30.2"))
  ;; The original is in `files`.
  (defun find-file-noselect (filename &optional nowarn rawfile wildcards)
    "Read file FILENAME into a buffer and return the buffer.
If a buffer exists visiting FILENAME, return that one, but
verify that the file has not changed since visited or saved.
The buffer is not selected, just returned to the caller.
Optional second arg NOWARN non-nil means suppress any warning messages,
and also don't verify the that the file has not been changed since
last visited or saved.
Optional third arg RAWFILE non-nil means the file is read literally.
Optional fourth arg WILDCARDS non-nil means do wildcard processing
and visit all the matching files.  When wildcards are actually
used and expanded, return a list of buffers that are visiting
the various files."
    (setq filename
          (abbreviate-file-name
           (expand-file-name filename)))
    (if (file-directory-p filename)
        (or (and find-file-run-dired
                 (run-hook-with-args-until-success
                  'find-directory-functions
                  (if find-file-visit-truename
                      (abbreviate-file-name (file-truename filename))
                    filename)))
            (error "%s is a directory" filename))
      (if (and wildcards
               find-file-wildcards
               (not (file-name-quoted-p filename))
               (string-match "[[*?]" filename))
          (let ((files (condition-case nil
                           (file-expand-wildcards filename t)
                         (error (list filename))))
                (find-file-wildcards nil))
            (if (null files)
                (find-file-noselect filename)
              (mapcar #'find-file-noselect files)))
        (let* ((buf (get-file-buffer filename))
               (truename (abbreviate-file-name (file-truename filename)))
               (attributes (file-attributes truename))
               (number (file-attribute-file-identifier attributes))
               ;; Find any buffer for a file that has same truename.
               (other (and (not buf)
                           (find-buffer-visiting
                            filename
                            ;; We want to filter out buffers that we've
                            ;; visited via symlinks and the like, where
                            ;; the symlink no longer exists.
                            (lambda (buffer)
                              (let ((file (buffer-local-value
                                           'buffer-file-name buffer)))
                                (and file (file-exists-p file))))))))
          ;; Let user know if there is a buffer with the same truename.
          (when other
            (or nowarn
                find-file-suppress-same-file-warnings
                (string-equal filename (buffer-file-name other))
                (files--message "%s and %s are the same file"
                                filename (buffer-file-name other)))
            ;; Optionally also find that buffer.
            (if (or find-file-existing-other-name find-file-visit-truename)
                (setq buf other)))
          ;; Check to see if the file looks uncommonly large.
          (when (not (or buf nowarn))
            (when (eq (abort-if-file-too-large
                       (file-attribute-size attributes) "open" filename
                       (not rawfile))
                      'raw)
              (setf rawfile t))
            (warn-maybe-out-of-memory (file-attribute-size attributes)))
          (if buf
              ;; We are using an existing buffer.
              (let (nonexistent)
                (or nowarn
                    (verify-visited-file-modtime buf)
                    (cond ((not (file-exists-p filename))
                           (setq nonexistent t)
                           (message "File %s no longer exists!" filename))
                          ;; Certain files should be reverted automatically
                          ;; if they have changed on disk and not in the buffer.
                          ((and (not (buffer-modified-p buf))
                                (let ((tail revert-without-query)
                                      (found nil))
                                  (while tail
                                    (if (string-match (car tail) filename)
                                        (setq found t))
                                    (setq tail (cdr tail)))
                                  found))
                           (with-current-buffer buf
                             (message "Reverting file %s..." filename)
                             (revert-buffer t t)
                             (message "Reverting file %s...done" filename)))
                          ((not query-about-changed-file)
                           (message
                            (substitute-command-keys
                             "File %s changed on disk.  \\[revert-buffer-quick] to load new contents%s")
                            (file-name-nondirectory filename)
                            (if (buffer-modified-p buf)
                                " and discard your edits"
                              "")))
                          ((yes-or-no-p
                            (if (string= (file-name-nondirectory filename)
                                         (buffer-name buf))
                                (format
                                 (if (buffer-modified-p buf)
                                     "File %s changed on disk.  Discard your edits? "
                                   "File %s changed on disk.  Reread from disk? ")
                                 (file-name-nondirectory filename))
                              (format
                               (if (buffer-modified-p buf)
                                   "File %s changed on disk.  Discard your edits in %s? "
                                 "File %s changed on disk.  Reread from disk into %s? ")
                               (file-name-nondirectory filename)
                               (buffer-name buf))))
                           (with-current-buffer buf
                             (revert-buffer t t)))))
                (with-current-buffer buf

                  ;; Check if a formerly read-only file has become
                  ;; writable and vice versa, but if the buffer agrees
                  ;; with the new state of the file, that is ok too.
                  (let ((read-only (not (file-writable-p buffer-file-name))))
                    (unless (or nonexistent
                                (eq read-only buffer-file-read-only)
                                (eq read-only buffer-read-only))
                      (when (or nowarn
                                (let* ((new-status
                                        (if read-only "read-only" "writable"))
                                       (question
                                        (format "File %s is %s on disk.  Make buffer %s, too? "
                                                buffer-file-name
                                                new-status new-status)))
                                  (y-or-n-p question)))
                        (setq buffer-read-only read-only)))
                    (setq buffer-file-read-only read-only))

                  (unless (or (eq (null rawfile) (null find-file-literally))
                              nonexistent
                              ;; It is confusing to ask whether to visit
                              ;; non-literally if they have the file in
                              ;; hexl-mode or image-mode.
                              (memq major-mode '(hexl-mode image-mode)))
                    (if (buffer-modified-p)
                        (if (let ((help-form
                                   (format-message
                                    (if rawfile "\
The file %s is already visited normally,
and you have edited the buffer.  Now you have asked to visit it literally,
meaning no coding system handling, format conversion, or local variables.
Emacs can visit a file in only one way at a time."
                                      "\
The file %s is already visited literally,
meaning no coding system handling, format conversion, or local variables.
You have edited the buffer.  Now you have asked to visit the file normally,
but Emacs can visit a file in only one way at a time.")
                                    (file-name-nondirectory filename))))
                              (y-or-n-p
                               (if rawfile "\
Do you want to save the file, and visit it literally instead? " "\
Do you want to save the file, and visit it normally instead? ")))
                            (progn
                              (save-buffer)
                              (find-file-noselect-1 buf filename nowarn
                                                    rawfile truename number))
                          (if (y-or-n-p
                               (if rawfile "\
Do you want to discard your changes, and visit the file literally now? " "\
Do you want to discard your changes, and visit the file normally now? "))
                              (find-file-noselect-1 buf filename nowarn
                                                    rawfile truename number)
                            (error (if rawfile "File already visited non-literally"
                                     "File already visited literally"))))
                      (if nomis/find-file-noselect--no-prompt-if-buffer-exists?
                          (pop-to-buffer buf)
                        (if (let ((help-form
                                   (format-message
                                    (if rawfile "\
The file %s is already visited normally.
You have asked to visit it literally,
meaning no coding system decoding, format conversion, or local variables.
But Emacs can visit a file in only one way at a time."
                                      "\
The file %s is already visited literally,
meaning no coding system decoding, format conversion, or local variables.
You have asked to visit it normally,
but Emacs can visit a file in only one way at a time.")
                                    (file-name-nondirectory filename))))
                              (y-or-n-p
                               (if rawfile "\
Do you want to revisit the file literally now? " "\
Do you want to revisit the file normally now? ")))
                            (find-file-noselect-1 buf filename nowarn
                                                  rawfile truename number)
                          (error (if rawfile "File already visited non-literally"
                                   "File already visited literally")))))))
                ;; Return the buffer we are using.
                buf)
            ;; Create a new buffer.
            (setq buf (create-file-buffer filename))
            ;; find-file-noselect-1 may use a different buffer.
            (find-file-noselect-1 buf filename nowarn
                                  rawfile truename number)))))))

 (t
  (message-box
   "You need to fix/check `find-file-noselect` for this version of Emacs.")))

(provide 'nomis-find-file-noselect-without-confusing-messages)
