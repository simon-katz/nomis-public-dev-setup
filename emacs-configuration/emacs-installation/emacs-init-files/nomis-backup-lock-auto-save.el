;;; Init stuff --- nomis-backup-lock-auto-save tailoring  -*- lexical-binding: t; -*-

;;; Code:

;;;; Require things

(require 'cl-lib)
(require 'dash)

;;;; TEMP

(setq vc-make-backup-files t)

;;;; File locations

(defconst nomis/backup-directory
  (expand-file-name (cl-ecase 2
                      (1 "~/.emacs-backups/")
                      (2 "~/development-100/.emacs-backups/"))))

(make-directory nomis/backup-directory t)
(setq backup-directory-alist
      `(("." . ,nomis/backup-directory)))

;;;; Shorten filenames

(defconst nomis/blau/max-filename-length 150) ; A bit arbitrary,
(defconst nomis/blau/sha1-length 40)

(defun nomis/blau/maybe-shorten-filename (filename)
  (if (<= (length filename)
          nomis/blau/max-filename-length)
      filename
    (let* ((n-chars-we-can-keep (- nomis/blau/max-filename-length
                                   nomis/blau/sha1-length
                                   1))
           (cut-off (- (length filename)
                       n-chars-we-can-keep))
           (first-part  (substring filename 0 cut-off))
           (second-part (substring filename cut-off))
           (result (concat (sha1 first-part) "-" second-part)))
      (let* ((inhibit-message t))
        (message "nomis/blau/maybe-shorten-filename: Shortened filename from %s chars to %s chars -- %s to %s"
                 (length filename)
                 (length result)
                 filename
                 result))
      result)))

;;;; Shorten auto-save file names

(defconst nomis/auto-save-directory
  (expand-file-name (cl-ecase 2
                      (1 "~/.emacs-auto-save/")
                      (2 "~/development-100/.emacs-auto-save/"))))

(make-directory nomis/auto-save-directory t)

(advice-add
 'make-auto-save-file-name
 :around
 (lambda (orig-fun &rest args)
   (let ((inhibit-message t))
     (message "make-auto-save-file-name %S" buffer-file-name))
   (cl-ecase 2
     (1 (apply orig-fun args))
     (2 (if (or (not buffer-file-name)
                (file-remote-p buffer-file-name))
            (apply orig-fun args)
          (let* ((dir  (file-name-directory buffer-file-name))
                 (base (-> (file-name-nondirectory buffer-file-name)
                           nomis/blau/maybe-shorten-filename))
                 (dir-hash (substring (secure-hash 'md5 dir) 0 8)))
            (expand-file-name (concat "#" dir-hash "_" base "#")
                              nomis/auto-save-directory))))))
 '((name . nomis/blau/shorten-auto-save-file-name)))

;;;; Shorten lock file names

(defconst nomis/lockfile-directory
  (expand-file-name (cl-ecase 2
                      (1 "~/.emacs-lockfiles/")
                      (2 "~/development-100/.emacs-lockfiles/"))))

(make-directory nomis/lockfile-directory t)

(advice-add
 'make-lock-file-name
 :around
 (lambda (orig-fun filename)
   (let ((inhibit-message t))
     (message "make-lock-file-name %S" filename))
   (cl-ecase 2
     (1 (funcall orig-fun filename))
     (2 (if (file-remote-p filename)
            (funcall orig-fun filename)
          (let* ((dir (file-name-directory filename))
                 (base (-> (file-name-nondirectory filename)
                           nomis/blau/maybe-shorten-filename))
                 (dir-hash (substring (secure-hash 'md5 dir) 0 8)))
            (expand-file-name (concat ".#" dir-hash "_" base)
                              nomis/lockfile-directory)))))
   )
 '((name . nomis/blau/hack-lock-file-name)))

;;; End

(provide 'nomis-backup-lock-auto-save)
