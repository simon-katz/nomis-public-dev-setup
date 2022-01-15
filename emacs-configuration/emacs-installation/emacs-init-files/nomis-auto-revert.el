;;;; nomis-auto-revert.el --- auto-revert-mode tailoring ---  -*- lexical-binding: t -*-

(require 'nomis-lightweight-objects)

;;;; ___________________________________________________________________________
;;;; ---- Standard tailoring ----

(setq auto-revert-verbose nil)

;;;; Make things more responsive when you cause a log file to update more than
;;;; once in quick succession.
(setq auto-revert-use-notify nil)
(setq auto-revert-interval 2)

;;;; ___________________________________________________________________________
;;;; ---- Extra behaviour when new content is added ----
;;;;
;;;; When at eob:
;;;; - Keep last line at bottom of window (not middle!).
;;;; - Pop up a message when content at eob changes.

;;;; _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
;;;; ---- Vars that can be tailored ----

(defvar nomis/auto-revert/revert-text
  "▶▶▶▶ Reverted buffer -- maybe a rollover happened ◀◀◀◀")

(defvar nomis/auto-revert/new-content-text/begin
  "▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼  New content  ▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼")

(defvar nomis/auto-revert/new-content-text/end
  "▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲  New content  ▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲")

(defvar nomis/auto-revert/n-chars-to-compare 1000
  "Number of characters to check at end of buffer to see whether
  auto-tailing has changed things.")

;;;; _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
;;;; ---- Hash table of recent buffer tails ----

(defvar -nomis/auto-revert/tail-infos (make-hash-table)
  "Hash table from buffer to most-recently seen last-n-chars of buffer.")

(defun -nomis/auto-revert/forget-buffer ()
  (remhash (current-buffer) -nomis/auto-revert/tail-infos))

(cl-defun -nomis/auto-revert/note-buffer-killed (&rest _)
  (-nomis/auto-revert/forget-buffer))

(add-hook 'kill-buffer-hook
          '-nomis/auto-revert/note-buffer-killed)

;;;; To help when debugging:
;;;;   (hash-table-count -nomis/auto-revert/tail-infos)
;;;;   (hash-table-keys -nomis/auto-revert/tail-infos)

(defun -nomis/auto-revert/get-tail-info (buffer)
  (gethash buffer -nomis/auto-revert/tail-infos))

(defun -nomis/auto-revert/put-tail-info (buffer info)
  (puthash buffer info -nomis/auto-revert/tail-infos))

;;;; _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
;;;; ---- -nomis/auto-revert/eob-stuff ----

(defun -nomis/auto-revert/buffer-file-size (&optional buffer) ; TODO: Move to a generic place.
  (let* ((buffer (or buffer (current-buffer))))
    (->> (buffer-file-name buffer)
         file-attributes
         (nth 7))))

(defun -nomis/auto-revert/revert-needed? (file-size eob prev-tail-info)
  "Return non-nil if a revert is needed. That's the case if
either:
(1) `file-size` is less than the previous `:file-size`, or
(2) the previous `:tail-chars` do not match the characters that are in the
buffer at the previous `:start-pos`.
This isn't perfect, but it's probably the best we can do."
  (-let* (((&hash :file-size  prev-file-size
                  :tail-chars prev-tail-chars
                  :start-pos  prev-start-pos
                  :eob        prev-eob)
           prev-tail-info))
    (or (< file-size prev-file-size)
        (not (equal prev-tail-chars
                    (buffer-substring-no-properties prev-start-pos
                                                    prev-eob))))))

(defun -nomis/auto-revert/prev-eob-if-buffer-changed ()
  ;; Return old eob if buffer has changed since previous call, otherwise nil.
  ;; If there's been a rollover, revert buffer and return nil.
  (let* ((file-size (-nomis/auto-revert/buffer-file-size)))
    (when (null file-size)
      (error "file-size is unexpectedly nil"))
    (let* ((prev-tail-info (-nomis/auto-revert/get-tail-info (current-buffer)))
           (eob (point-max)))
      (if (and prev-tail-info
               (-nomis/auto-revert/revert-needed? file-size eob prev-tail-info))
          (progn
            ;; The file has changed and not simply been appended to; /eg/
            ;; a log rollover.
            (message "Reverting buffer (perhaps a rollover happened): %s"
                     (buffer-name))
            (-nomis/auto-revert/forget-buffer)
            (revert-buffer t t t)
            (let* ((pos (save-excursion (beginning-of-line) (point))))
              (nomis/popup/message-v2 t pos nomis/auto-revert/revert-text))
            nil)
        (let* ((start-pos (max 1
                               (- eob nomis/auto-revert/n-chars-to-compare)))
               (tail-chars (buffer-substring-no-properties start-pos eob))
               (tail-info ($$ :file-size  file-size
                              :start-pos  start-pos
                              :eob        eob
                              :tail-chars tail-chars)))
          (-nomis/auto-revert/put-tail-info (current-buffer) tail-info)
          (when prev-tail-info
            (let* ((prev-eob ($ :eob prev-tail-info)))
              (when (not (eq prev-eob eob))
                prev-eob))))))))

(defun -nomis/auto-revert/extras-for-buffer ()
  ;; If at eob and buffer has changed since previous call:
  ;; - Bottomise (ie scroll so that eob is at bottom of window).
  ;; - Temporarily highlight the changes.
  ;; TODO: We are confusing general auto-reversion with log file tailing with
  ;;       what we get from
  ;;       `(setq logview-auto-revert-mode 'auto-revert-tail-mode)`?
  (when (eq major-mode 'logview-mode) ; TODO: Move all to `nomis-logview`.
    (when-let (prev-eob (-nomis/auto-revert/prev-eob-if-buffer-changed))
      (when (= (point) (point-max))
        (recenter-top-bottom -1))
      (let* ((begin-pos (save-excursion (goto-char prev-eob)
                                        (forward-line -1)
                                        (point)))
             (begin-message nomis/auto-revert/new-content-text/begin)
             (end-pos (point-max))
             (end-message   nomis/auto-revert/new-content-text/end))
        (nomis/popup/message-v2 t begin-pos begin-message)
        (nomis/popup/message-v2 t end-pos   end-message)))))

(defun -nomis/auto-revert-extras (&rest _)
  ;; TODO: Why are we dealing with windows here? If we do need
  ;;       `with-selected-window`, we should avoid doing work for the same
  ;;       buffer multiple times.
  (dolist (w (get-buffer-window-list nil nil t))
    (with-selected-window w
      (-nomis/auto-revert/extras-for-buffer))))

(let* ((advice-name '-nomis/auto-revert/eob-stuff))
  (advice-add
   'auto-revert-handler
   :after
   '-nomis/auto-revert-extras
   `((name . ,advice-name))))

(when nil ; Code to remove advice when in dev.
  (advice-remove 'auto-revert-handler '-nomis/auto-revert/eob-stuff)
  )

;;;; ___________________________________________________________________________

(provide 'nomis-auto-revert)
