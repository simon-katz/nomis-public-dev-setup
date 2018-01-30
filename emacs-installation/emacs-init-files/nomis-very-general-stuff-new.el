;;;; Init stuff -- Very general stuff (new).
;;;; - The plan is to move stuff to here from "nomis-very-general-stuff", but
;;;;   I don't want to do that right now in case I break things.
;;;;   And I need what's here earlier in the init setup.

;;;; ___________________________________________________________________________

(defun nomis/y-or-n-p-with-quit->nil (prompt)
  (condition-case nil
      (y-or-n-p prompt)
    (quit nil)))

;;;; ___________________________________________________________________________

(cl-defmacro nomis/with-temporary-invisible-changes (() &rest forms)
  ;; Copied from https://www.emacswiki.org/emacs/UndoCommands, and changed.
  "Executes FORMS with a temporary buffer-undo-list, undoing on return.
The changes you make within FORMS are undone before returning.
But more importantly, the buffer's buffer-undo-list is not affected.
This allows you to temporarily modify read-only buffers too."
  (declare (indent 1))
  `(let* ((buffer-undo-list)
          (modified (buffer-modified-p))
          (inhibit-read-only t))
     (save-excursion
       (unwind-protect
           (progn ,@forms)
         (primitive-undo (length buffer-undo-list) buffer-undo-list)
         (set-buffer-modified-p modified)))
     nil))

;;;; ___________________________________________________________________________

(provide 'nomis-very-general-stuff-new)
