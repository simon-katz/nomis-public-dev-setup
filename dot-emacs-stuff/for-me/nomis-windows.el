;;;; Init stuff -- Windows.

;;;; ___________________________________________________________________________
;;;; ---- winner-mode ----
;;;; winner-mode has `winner-undo` to restore windows after you accidentally
;;;; close them with e.g. C-x 1.  Also `winner-redo`.

(winner-mode 1)

;;;; ___________________________________________________________________________
;;;; ---- Shift-up/down/left/right to navigate between windows ----

(windmove-default-keybindings)

;;;; ___________________________________________________________________________
;;;; ---- Cycle windows, across frames ----

(define-key global-map [(control tab)] 'next-multiframe-window)
(define-key global-map [(control shift tab)] 'previous-multiframe-window)

;;;; ___________________________________________________________________________
;;;; ---- Fiddling with windows ----

;;;; ---------------------------------------------------------------------------
;;;; ---- transpose-frame ----
;;;; Swap windows around within a frame.

(require 'transpose-frame)

;;;; ---------------------------------------------------------------------------
;;;; ---- Swap buffers between windows ----

;;;; Based on something I found at
;;;; http://www.emacswiki.org/emacs/TransposeWindows.

(defvar swapping-buffer nil)
(defvar swapping-window nil)

(defun swap-buffers-in-windows-setup ()
  "Swap buffers between two windows -- setup"
  (interactive)
  (setq swapping-buffer (current-buffer))
  (setq swapping-window (selected-window))
  (message "Buffer and window marked for swapping."))

(defun swap-buffers-in-windows-do-it ()
  "Swap buffers between two windows -- do it"
  (interactive)
  (if (and swapping-window
           swapping-buffer)
      (let ((this-buffer (current-buffer))
            (this-window (selected-window)))
        (if (and (window-live-p swapping-window)
                 (buffer-live-p swapping-buffer))
            (progn (switch-to-buffer swapping-buffer)
                   (select-window swapping-window)
                   (switch-to-buffer this-buffer)
                   (select-window this-window)
                   (message "Swapped buffers."))
          (message "Old buffer/window killed.  Aborting."))
        (setq swapping-window this-window) ; allow for a chain of swaps
        )
    (error "Need to do `swap-buffers-in-windows-setup` first.")))

(global-set-key (kbd "C-c C--") 'swap-buffers-in-windows-setup)
(global-set-key (kbd "C-c C-=") 'swap-buffers-in-windows-do-it)

;;;; ___________________________________________________________________________

(provide 'nomis-windows)
