;;; Init stuff --- nomis-backup-lock-auto-save tailoring  -*- lexical-binding: t; -*-

;;; Code:

;;;; Require things

(require 'cl-lib)

;;;; File locations

(defconst nomis/backup-directory
  (expand-file-name (cl-ecase 2
                      (1 "~/.emacs-backups/")
                      (2 "~/development-100/.emacs-backups/"))))

(make-directory nomis/backup-directory t)
(setq backup-directory-alist
      `(("." . ,nomis/backup-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,nomis/backup-directory t)))

(defconst nomis/lockfile-directory
  (expand-file-name (cl-ecase 2
                      (1 "~/.emacs-lockfiles/")
                      (2 "~/development-100/.emacs-lockfiles/"))))

(make-directory nomis/lockfile-directory t)
(setq lock-file-name-transforms `((".*" ,nomis/lockfile-directory t)))

;;;; Prevent auto-save file names being too long

;; You were having problems when editing the following namespace:
;;   `com.nomistech.clojure-the-language.c-950-tools-stuff.s-100-linting.ss-0400-nested-lets-to-demo-highlighting-test`

;; Based on code from the following places:
;; - https://emacs.stackexchange.com/questions/48301/spacemacs-and-file-name-too-long-error-on-auto-save
;; - https://www.reddit.com/r/emacs/comments/t07e7e/file_name_too_long_error/

(defconst nomis/blau/max-filename-length 150) ; A bit arbitrary,
(defconst nomis/blau/sha1-length 40)

(defun nomis/blau/maybe-shorten-filename (filename)
  (if (<= (length filename)
          nomis/blau/max-filename-length)
      filename
    (let* ((n-chars-we-can-keep (- nomis/blau/max-filename-length
                                   nomis/blau/sha1-length
                                   1) )
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

(cond
 ((member emacs-version
          '("28.1"
            "28.2"
            "29.4"
            "30.1"
            "30.2"))
  (advice-add 'make-auto-save-file-name
              :around
              (lambda (orig-fun &rest args)
                (let* ((buffer-file-name
                        (when buffer-file-name
                          (-> buffer-file-name
                              nomis/blau/maybe-shorten-filename))))
                  (apply orig-fun args)))
              '((name . nomis/blau/shorten-file-name))))

 (t
  (message-box
   "You need to fix/check `make-auto-save-file-name` for this version of Emacs.")))

;; (advice-remove 'make-auto-save-file-name 'nomis/blau/shorten-file-name)

;;; End

(provide 'nomis-backup-lock-auto-save)
