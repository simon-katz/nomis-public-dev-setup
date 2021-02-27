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

(cl-defun -nomis/auto-revert/note-buffer-killed (&rest _)
  (remhash (current-buffer) -nomis/auto-revert/tails))

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

(defun -nomis/auto-revert/old-eob-if-buffer-changed ()
  ;; Return old eob if buffer has changed since previous call, otherwise nil.
  (cl-multiple-value-bind (previous-tail-chars
                           old-eob)
      (-nomis/auto-revert/get-tail-info (current-buffer))
    (let* ((pmax (point-max))
           (current-tail-chars (buffer-substring-no-properties
                                (max 0
                                     (- pmax
                                        nomis/auto-revert/n-chars-to-compare))
                                pmax)))
      (-nomis/auto-revert/put-tail-info (current-buffer)
                                        (list current-tail-chars
                                              pmax))
      (when (and previous-tail-chars
                 (not (equal previous-tail-chars
                             current-tail-chars)))
        old-eob))))

(defun -nomis/auto-revert/maybe-popup-new-content-message ()
  ;; If buffer has changed since previous call, pop up a temporary message.
  (when-let (old-eob (-nomis/auto-revert/old-eob-if-buffer-changed))
    (let* ((begin-pos (save-excursion (goto-char old-eob)
                                      (forward-line -1)
                                      (point)))
           (begin-message nomis/auto-revert/new-content-text/begin)
           (end-pos (point-max))
           (end-message   nomis/auto-revert/new-content-text/end))
      (nomis/popup/message-v2 t begin-pos begin-message)
      (nomis/popup/message-v2 t end-pos   end-message))))

(defun -nomis/auto-revert/bottomise (&rest _)
  ;; For all windows showing current buffer:
  ;; - If at eob show eob at bottom of window.
  ;; - If buffer has changed pop up a temporary message.
  (dolist (w (get-buffer-window-list nil nil t))
    (with-selected-window w
      (when (= (point) (point-max))
        (recenter-top-bottom -1)
        (-nomis/auto-revert/maybe-popup-new-content-message)))))

(let* ((advice-name '-nomis/auto-revert/eob-stuff))
  (advice-add
   'auto-revert-handler
   :after
   '-nomis/auto-revert/bottomise
   `((name . ,advice-name))))

(when nil ; Code to remove advice when in dev.
  (advice-remove 'auto-revert-handler '-nomis/auto-revert/eob-stuff)
  )

;;;; ___________________________________________________________________________

(provide 'nomis-auto-revert)
