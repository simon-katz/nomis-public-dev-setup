;;;; Init stuff -- homeless-non-lexical -- to maybe move to a better place

;;;; TODO: Move homeless stuff to somewhere appropriate.

(require 'dash)

;;;; ___________________________________________________________________________

(defun nomis/untabify-buffer ()
  (interactive)
  (save-excursion
    (untabify (point-min) (point-max))))

(global-set-key [f11] 'nomis/untabify-buffer)

;;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(advice-add
 'org-indent-region
 :around
 (lambda (orig-fun &rest args)
   (cl-flet ((hack-to-make-undo-not-multistage
              ()
              ;; Without this, undo is multi-stage for `nomis/indent-buffer`
              ;; with weird jumping around the buffer.
              ;; I've tried many things that don't fix the problem, including:
              ;; - calling `undo-boundary`
              ;; - binding `undo-inhibit-record-point`
              ;; - manually removing `nil`s from `buffer-undo-list`.
              ;; But I can't get any of those to work.
              ;; This "no-op" makes undo behave nicely (but I don't understand
              ;; why):
              (let* ((text "hack-to-make-undo-behave-nicely"))
                (insert text)
                (delete-char (- (length text))))))
     (hack-to-make-undo-not-multistage)
     (apply orig-fun args)))
 '((name . nomis/hack-to-make-undo-not-multistage)))

(defun nomis/indent-buffer ()
  (interactive)
  (nomis/with-atomic-undo
    (save-excursion
      (unless (member major-mode '(yaml-mode)) ; serious hack
        (indent-region (point-min) (point-max) nil))
      (untabify (point-min) (point-max))
      (delete-trailing-whitespace))))

(global-set-key [f12] 'nomis/indent-buffer)

;;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(defun nomis/git-dirty? ()
  (let* ((default-directory (nomis/dirtree/vc-root-dir))
         (output (shell-command-to-string
                  "test -z \"$(git status --porcelain)\" || echo -n \"dirty\""))
         (dirty? (equal output "dirty")))
    dirty?))

(defun nomis/clojure-files-in-repo ()
  (let* ((root-dir (nomis/dirtree/vc-root-dir))
         (filenames (directory-files-recursively root-dir
                                                 (nomis/rx/or
                                                  "\.clj[sxc]?$"
                                                  "\.edn$"))))
    filenames))

(defun nomis/indent-all-clj-files-in-project (force-when-dirty?)
  (interactive "P")
  (when (and (not force-when-dirty?)
             (nomis/git-dirty?))
    (error (s-join " "
                   '("I won't indent all files when the Git repo is dirty."
                     "Use a prefix arg to force."))))
  (let* ((filenames (nomis/clojure-files-in-repo)))
    (dolist (filename filenames)
      (message "Indenting %s" filename)
      (let* ((existing-buffer? (find-buffer-visiting filename))
             (buffer (find-file-noselect filename)))
        (with-current-buffer buffer
          (let* ((inhibit-message t))
            (nomis/indent-buffer))
          (save-buffer))
        (unless existing-buffer?
          (kill-buffer buffer)))))
  (when (featurep 'magit) (magit-refresh))
  (message "Finished indenting all clj files in project."))

(defun nomis/indent-all-clj-files-in-project-and-commit (force-when-dirty?)
  (interactive "P")
  (let* ((committed? nil)
         (root-dir (nomis/dirtree/vc-root-dir)))
    (nomis/indent-all-clj-files-in-project force-when-dirty?)
    (let* ((default-directory root-dir))
      (shell-command-to-string "git add .")
      (when (nomis/git-dirty?)
        (shell-command-to-string "git commit -m apply-local-formatting")
        (setq committed? t)))
    (when (featurep 'magit) (magit-refresh))
    (if committed?
        (message "Finished indenting all clj files in project and committing.")
      (message "Repo is clean after reformatting -- not commiting"))))

;;;; ___________________________________________________________________________

(defun nomis/timestamp-yyyy-mm-dd--hh-mm-ss ()
  (format-time-string "%Y-%m-%d--%H-%M-%S"))

(defun nomis/timestamp (kind)
  (cl-case kind
    (:date
     (format-time-string "%Y-%m-%d"))
    (:date-time
     (format-time-string "%Y-%m-%dT%H:%M:%S"))
    ((:date-time-zone t)
     (let ((timezone (format-time-string "%z")))
       (format "%s%s:%s"
               (nomis/timestamp :date-time)
               (substring timezone 0 3)
               (substring timezone 3 5))))))

(defun nomis/timestamp-no-t (kind)
  (cl-case kind
    (:date
     (format-time-string "%Y-%m-%d"))
    (:date-time
     (format-time-string "%Y-%m-%d %H:%M:%S"))
    ((:date-time-zone t)
     (let ((timezone (format-time-string "%z")))
       (format "%s%s:%s"
               (nomis/timestamp-no-t :date-time)
               (substring timezone 0 3)
               (substring timezone 3 5))))))

(defun nomis/insert-timestamp (p)
  (interactive "P")
  (insert (nomis/timestamp (cl-case (prefix-numeric-value p)
                             (1 :date)
                             (4 :date-time)
                             (t t)))))

;;;; ___________________________________________________________________________
;;;; nomis/set-up-devvy-windows-for-clj-and-jack-in

(defun -nomis/double-h-max-w ()
  (nomis/w-double)
  (maximize-frame-vertically))

(defun nomis/set-up-devvy-windows-for-clj-and-jack-in (&optional prefix)
  (interactive "P")
  (progn
    (unless prefix
      (cider-jack-in-clj nil))
    (-nomis/double-h-max-w)
    (let* ((main-monitor-width (nomis/main-monitor-width)))
      (cl-case main-monitor-width
        (2560 (modify-frame-parameters
               (selected-frame)
               `((left . 700))))
        (t (nomis/move-frame-to-screen-right 0))))
    (split-window-horizontally)
    (nomis/dirtree/goto-file/return-to-window)
    (when (fboundp 'flop-frame) (flop-frame)) ; I don't know why this is needed
    ;; Suddenly `(make-frame-command)` is causing a crash. Perhaps it happened
    ;; when I upgraded to macOS Ventura. Fix with `run-at-time`.
    ;; - SK 2023-10-08
    (run-at-time
     0.1
     nil
     (lambda ()
       (cl-flet ()
         (make-frame-command)
         (-nomis/double-h-max-w)
         (switch-to-buffer (messages-buffer))
         (split-window-vertically)
         (other-window 1)
         (let* ((nrepl-server-buffer (-find (lambda (b) (s-starts-with? "*nrepl-server"
                                                                        (buffer-name b)))
                                            (buffer-list))))
           (if nrepl-server-buffer
               (switch-to-buffer nrepl-server-buffer)
             (message "Didn't find nrepl-server-buffer"))))))))

;;;; ___________________________________________________________________________

(provide 'homeless-non-lexical)
