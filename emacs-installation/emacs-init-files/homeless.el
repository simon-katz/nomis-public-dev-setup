;;;; Init stuff -- homeless -- to maybe move to a better place

;;;; TODO: Move homeless stuff to somewhere appropriate.

;;;; ___________________________________________________________________________
;;;; Reverting buffers.
;;;; Copy-and-hack of http://www.emacswiki.org/emacs/RevertBuffer.

(defun _revert-all-buffers (buffer-predicate)
  (let (failures '())
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (when (and (buffer-file-name) (funcall buffer-predicate buf))
          (condition-case e
              (revert-buffer t t t)
            (error
             (push (buffer-file-name) failures)
             (message "%s" e)
             (beep))))))
    (message "Refreshed open files. %s"
             (if failures
                 (s-join " " (cons "Failures: " failures))
               ""))))

(defun revert-all-unmodified-buffers ()
  "Refreshes all open unmodified buffers from their files."
  (interactive)
  (_revert-all-buffers (lambda (b) (not (buffer-modified-p b)))) )

(defun revert-all-modified-buffers ()
  "Refreshes all open modified buffers from their files."
  ;; Copied from http://www.emacswiki.org/emacs/RevertBuffer, and renamed.
  (interactive)
  (when (y-or-n-p "Really revert modified buffers? You will lose stuff.")
    (_revert-all-buffers 'buffer-modified-p)) )

(defun revert-all-unmodified-buffers-in-git-repo ()
  "Refreshes all open unmodified buffers in current buffer's Git repo
 from their files."
  (interactive)
  (_revert-all-buffers (lambda (b)
                         (and (not (buffer-modified-p b))
                              (magit-auto-revert-repository-buffer-p b)))))

;;;; ___________________________________________________________________________

(defun nomis/untabify-buffer ()
  (interactive)
  (save-excursion
    (untabify (point-min) (point-max))))

(global-set-key [f11] 'nomis/untabify-buffer)

;;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(defun nomis/indent-buffer ()
  (interactive)
  (save-excursion
    (delete-trailing-whitespace)
    (unless (member major-mode '(yaml-mode)) ; serious hack
      (indent-region (point-min) (point-max) nil))
    (untabify (point-min) (point-max))))

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
                                                 "\.clj[sxc]?$")))
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

(defun nomis/timestamp (kind)
  (case kind
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

(defun nomis/insert-timestamp (p)
  (interactive "P")
  (insert (nomis/timestamp (case (prefix-numeric-value p)
                             (1 :date)
                             (4 :date-time)
                             (t t)))))

;;;; ___________________________________________________________________________
;;;; nomis/set-up-devvy-windows-for-clj-and-jack-in

(defun nomis/set-up-devvy-windows-for-clj-and-jack-in (&optional prefix)
  (interactive "P")
  (cl-flet ((double-h-max-w ()
                            (nomis/w-double)
                            (maximize-frame-vertically)))
    (unless prefix
      (cider-jack-in-clj nil))
    (double-h-max-w)
    (nomis/move-frame-to-screen-right 0)
    (split-window-horizontally)
    (nomis/dirtree/goto-file/return-to-window)
    (when (fboundp 'flop-frame) (flop-frame)) ; I don't know why this is needed
    (make-frame-command)
    (double-h-max-w)
    (switch-to-buffer (messages-buffer))
    (split-window-vertically)
    (other-window 1)
    (let* ((nrepl-server-buffer (-find (lambda (b) (s-starts-with? "*nrepl-server"
                                                                   (buffer-name b)))
                                       (buffer-list))))
      (if nrepl-server-buffer
          (switch-to-buffer nrepl-server-buffer)
        (message "Didn't find nrepl-server-buffer")))))

;;;; ___________________________________________________________________________

(provide 'homeless)
