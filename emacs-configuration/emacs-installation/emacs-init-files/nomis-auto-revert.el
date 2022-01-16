;;;; nomis-auto-revert.el --- auto-revert-mode tailoring ---  -*- lexical-binding: t -*-

(require 'nomis-lightweight-objects)
(require 'nomis-buffers-windows-frames)

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

(defvar nomis/auto-revert/new-content-text
  "▶▶▶▶ New content added to bottom of buffer ◀◀◀◀")

(defvar nomis/auto-revert/new-content-text/begin
  "▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼  New content  ▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼")

(defvar nomis/auto-revert/new-content-text/end
  "▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲  New content  ▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲")

(defvar nomis/auto-revert/n-chars-to-compare 1000
  "Number of characters to check at end of buffer to see whether
  auto-tailing has changed things.")

(defface -nomis/auto-revert/new-text-highlight-face
  `((t (:background "PaleGreen1")))
  "Face used for highlighting new text.")

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
;;;; ---- -nomis/logview-auto-revert-extras ----

(defun -nomis/auto-revert/buffer-file-size (&optional buffer) ; TODO: Move to a generic place.
  (let* ((buffer (or buffer (current-buffer))))
    (->> (buffer-file-name buffer)
         file-attributes
         (nth 7))))

(defun -nomis/auto-revert/probable-rollover? (file-size prev-tail-info)
  "Return non-nil if a revert is needed. That's the case if
either:
(1) `file-size` is less than the previous `:file-size`, or
(2) the previous `:tail-chars` do not match the characters that are in the
buffer at the previous `:start-pos`.
This isn't perfect, but it's probably the best we can do."
  (and prev-tail-info
       (-let* (((&hash :file-size  prev-file-size
                       :tail-chars prev-tail-chars
                       :start-pos  prev-start-pos
                       :eob        prev-eob)
                prev-tail-info))
         (or (< file-size prev-file-size)
             (not (equal prev-tail-chars
                         (buffer-substring-no-properties prev-start-pos
                                                         prev-eob)))))))

;; TODO: Pass in `buffer` from the very-outside.
(defun -nomis/auto-revert/prev-eob-or-change-desc ()
  ;; Determine how `buffer` and its file have changed since the previous call of
  ;; this function for `buffer`, and:
  ;; - If there has been a rollover, return `:rollover`. (The caller is expected
  ;;   to revert the buffer.)
  ;; - If this is the first call or the first call since a rollover was detected
  ;;   return `nil`.
  ;; - If `buffer` has not changed, return `nil`.
  ;; - Otherwise return the previous eob. (The caller is expected to highlight
  ;;   the new buffer content.)
  (let* ((file-size (-nomis/auto-revert/buffer-file-size)))
    (when (null file-size)
      (error "file-size is unexpectedly nil"))
    (let* ((prev-tail-info (-nomis/auto-revert/get-tail-info (current-buffer))))
      (if (-nomis/auto-revert/probable-rollover? file-size prev-tail-info)
          (progn
            (-nomis/auto-revert/forget-buffer)
            :rollover)
        (let* ((eob (point-max))
               (start-pos (max 1
                               (- eob nomis/auto-revert/n-chars-to-compare)))
               (tail-chars (buffer-substring-no-properties start-pos eob))
               (tail-info ($$ :file-size  file-size
                              :start-pos  start-pos
                              :eob        eob
                              :tail-chars tail-chars)))
          (-nomis/auto-revert/put-tail-info (current-buffer) tail-info)
          (when prev-tail-info
            (let* ((prev-eob ($ :eob prev-tail-info)))
              (unless (eq prev-eob eob)
                prev-eob))))))))

(defun -nomis/auto-revert/extras-for-buffer ()
  ;; If there have been no changes, do nothing
  ;; If a rollover has happeneed, revert the buffer.
  ;; Otherwise:
  ;; - If point is at eob, scroll so that eob is the bottom line of the window.
  ;; - Temporarily highlight the changes.
  ;; TODO: We are confusing general auto-reversion with log file tailing with
  ;;       what we get from
  ;;       `(setq logview-auto-revert-mode 'auto-revert-tail-mode)`?
  (when (eq major-mode 'logview-mode) ; TODO: Move all to `nomis-logview`.
    (let* ((prev-eob-or-change-desc
            (-nomis/auto-revert/prev-eob-or-change-desc)))
      (cond ((null prev-eob-or-change-desc)
             ;; Nothing to do.
             )
            ((eq prev-eob-or-change-desc :rollover)
             (progn
               (message "Reverting buffer because a rollover happened): %s"
                        (buffer-name))
               (revert-buffer t t t)
               (nomis/foreach-buffer-window
                (current-buffer)
                (lambda ()
                  (let* ((bol (save-excursion (beginning-of-line) (point))))
                    (nomis/popup/message-v2 t bol nomis/auto-revert/revert-text))))
               nil))
            (t
             (let* ((prev-eob prev-eob-or-change-desc)
                    (begin-pos (save-excursion (goto-char prev-eob)
                                               (forward-line -1)
                                               (point))))
               ;; Highlight the changes.
               (let* ((begin-message nomis/auto-revert/new-content-text/begin)
                      (end-pos (point-max))
                      (end-message   nomis/auto-revert/new-content-text/end))
                 (nomis/popup/message-v2 t begin-pos begin-message)
                 (nomis/popup/message-v2 t end-pos   end-message)
                 (let* ((face '-nomis/auto-revert/new-text-highlight-face))
                   (nomis/popup/display-temp-overlay prev-eob
                                                     end-pos
                                                     'face face)))
               (nomis/foreach-buffer-window
                (current-buffer)
                (lambda ()
                  ;; Maybe scroll.
                  (when (= (point) (point-max))
                    (recenter-top-bottom -1))
                  (when (>= begin-pos (window-end nil t))
                    ;; Say that text has been added at end of file.
                    (let* ((bol (save-excursion (beginning-of-line) (point))))
                      (nomis/popup/message-v2 t
                                              bol
                                              nomis/auto-revert/new-content-text)))))))))))

(defun -nomis/auto-revert-extras (&rest _)
  (dolist (b (buffer-list))
    (with-current-buffer b
      (-nomis/auto-revert/extras-for-buffer))))

(let* ((advice-name '-nomis/logview-auto-revert-extras))
  (advice-add
   'auto-revert-handler
   :after
   '-nomis/auto-revert-extras
   `((name . ,advice-name))))

(when nil ; Code to remove advice when in dev.
  (advice-remove 'auto-revert-handler '-nomis/logview-auto-revert-extras)
  )

;;;; ___________________________________________________________________________

(provide 'nomis-auto-revert)
