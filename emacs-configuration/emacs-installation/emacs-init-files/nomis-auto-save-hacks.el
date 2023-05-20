;;;; Init stuff -- Nomis auto-save hacks --  -*- lexical-binding: t -*-

;;;; ___________________________________________________________________________
;;;; Prevent auto-save file names being too long.

;;;; You were having problems when editing the following namespace:
;;;;   `com.nomistech.clojure-the-language.c-950-tools-stuff.s-100-linting.ss-0400-nested-lets-to-demo-highlighting-test`

;;;; Based on code from the following places:
;;;; - https://emacs.stackexchange.com/questions/48301/spacemacs-and-file-name-too-long-error-on-auto-save
;;;; - https://www.reddit.com/r/emacs/comments/t07e7e/file_name_too_long_error/

(defconst nomis/auto-save-hacks/max-filename-length 200) ; A bit arbitrary,
(defconst nomis/auto-save-hacks/sha1-length 40)

(defun nomis/auto-save-hacks/maybe-shorten-filename (filename)
  (if (<= (length filename)
          nomis/auto-save-hacks/max-filename-length)
      filename
    (let* ((n-chars-we-can-keep (- nomis/auto-save-hacks/max-filename-length
                                   nomis/auto-save-hacks/sha1-length
                                   1) )
           (cut-off (- (length filename)
                       n-chars-we-can-keep))
           (first-part  (substring filename 0 cut-off))
           (second-part (substring filename cut-off))
           (result (concat (sha1 first-part) "-" second-part)))
      (let* ((inhibit-message t))
        (message "nomis/auto-save-hacks/maybe-shorten-filename: Shortened filename from %s chars to %s chars -- %s to %s"
                 (length filename)
                 (length result)
                 filename
                 result))
      result)))

(cond
 ((member emacs-version
          '("28.1"
            "28.2"))
  (advice-add 'make-auto-save-file-name
              :around
              (lambda (orig-fun &rest args)
                (let* ((buffer-file-name
                        (when buffer-file-name
                          (-> buffer-file-name
                              nomis/auto-save-hacks/maybe-shorten-filename))))
                  (apply orig-fun args)))
              '((name . nomis/auto-save-hacks/shorten-file-name))))

 (t
  (message-box
   "You need to fix/check `make-auto-save-file-name` for this version of Emacs.")))

;; (advice-remove 'make-auto-save-file-name 'nomis/auto-save-hacks/shorten-file-name)

;;;; ___________________________________________________________________________

(provide 'nomis-auto-save-hacks)
