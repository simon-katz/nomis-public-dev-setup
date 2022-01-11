;;;; nomis-auto-revert.el --- auto-revert-mode tailoring ---  -*- lexical-binding: t -*-

;;;; ___________________________________________________________________________
;;;; ---- Standard tailoring ----

(setq auto-revert-verbose nil)

;;;; Make things more responsive when you cause a log file to update more than
;;;; once in quick succession.
(setq auto-revert-use-notify nil)
(setq auto-revert-interval 1)

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

(defvar -nomis/auto-revert/tails (make-hash-table)
  "Hash table from buffer to most-recently seen last-n-chars of buffer.")

(defun -nomis/auto-revert/forget-buffer ()
  (remhash (current-buffer) -nomis/auto-revert/tails))

(cl-defun -nomis/auto-revert/note-buffer-killed (&rest _)
  (-nomis/auto-revert/forget-buffer))

(add-hook 'kill-buffer-hook
          '-nomis/auto-revert/note-buffer-killed)

;;;; To help when debugging:
;;;;   (hash-table-count -nomis/auto-revert/tails)
;;;;   (hash-table-keys -nomis/auto-revert/tails)

(defun -nomis/auto-revert/get-tail-info (buffer)
  (gethash buffer -nomis/auto-revert/tails))

(defun -nomis/auto-revert/put-tail-info (buffer info)
  (puthash buffer info -nomis/auto-revert/tails))

;;;; _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
;;;; ---- -nomis/auto-revert/eob-stuff ----

(defun -nomis/auto-revert/buffer-file-size (&optional buffer) ; TODO: Move to a generic place.
  (let* ((buffer (or buffer (current-buffer))))
    (->> (buffer-file-name buffer)
         file-attributes
         (nth 7))))

(defun -nomis/auto-revert/old-eob-if-buffer-changed ()
  ;; Return old eob if buffer has changed since previous call, otherwise nil.
  ;; If there's been a rollover (but see TODO below), revert buffer and
  ;; return nil.
  ;; TODO: About rollover: We revert the buffer if the log file reduces in size.
  ;;       This doesn't catch rollovers when the old file was smaller than the
  ;;       current file, which can happen. So there is more to do here if we
  ;;       want something perfect!
  ;; TODO: We no longer use `previous-tail-chars`, so we don't need to save
  ;;       that info.
  (cl-multiple-value-bind (previous-tail-chars
                           old-eob
                           old-file-size)
      (-nomis/auto-revert/get-tail-info (current-buffer))
    (let* ((current-file-size (-nomis/auto-revert/buffer-file-size)))
      (if (and current-file-size ; nil if file has been deleted
               old-eob
               (< current-file-size old-file-size))
          (progn
            ;; The file has changed and not simply been appended to; /eg/
            ;; a log rollover.
            (message "Fully reverting buffer (perhaps a rollover happened): %s"
                     (buffer-name))
            (-nomis/auto-revert/forget-buffer)
            (revert-buffer t t t)
            (let* ((pos (save-excursion (beginning-of-line) (point))))
              (nomis/popup/message-v2 t pos nomis/auto-revert/revert-text))
            nil)
        (let* ((pmax (point-max))
               (current-tail-chars (buffer-substring-no-properties
                                    (max 1
                                         (- pmax
                                            nomis/auto-revert/n-chars-to-compare))
                                    pmax)))
          (-nomis/auto-revert/put-tail-info (current-buffer)
                                            (list current-tail-chars
                                                  pmax
                                                  current-file-size))
          (when (and old-eob
                     (not (eq old-eob pmax)))
            old-eob))))))

(defun -nomis/auto-revert/extras-for-buffer ()
  ;; If at eob and buffer has changed since previous call:
  ;; - Bottomise (ie scroll so that eob is at bottom of window).
  ;; - Temporarily highlight the changes.
  ;; TODO: We are confusing general auto-reversion with log file tailing with
  ;;       what we get from
  ;;       `(setq logview-auto-revert-mode 'auto-revert-tail-mode)`?
  (when (and (eq major-mode 'logview-mode) ; TODO: Move all to `nomis-logview`.
             (= (point) (point-max)))
    (when-let (old-eob (-nomis/auto-revert/old-eob-if-buffer-changed))
      (recenter-top-bottom -1)
      (let* ((begin-pos (save-excursion (goto-char old-eob)
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
