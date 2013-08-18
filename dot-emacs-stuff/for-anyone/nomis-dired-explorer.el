;;;; Nomis Dired Explorer -- A Simple File Explorer.
;;;;
;;;; Navigate in one window; display contents in other window in the
;;;; same frame.  (That's "window" and "frame" in Emacs terminology.
;;;; See http://www.gnu.org/software/emacs/manual/html_node/elisp/Frames.html.)
;;;;
;;;; See my blog post at
;;;; http://nomistech.blogspot.co.uk/2013/06/simple-directory-navigation-for-emacs.html
;;;;
;;;; How this works:
;;;;
;;;; In a dired buffer:
;;;;
;;;; - M-<up/down/left/right>
;;;;   Navigate around without updating the other window.
;;;;   Most of these are simply new bindings for existing dired
;;;;   commands.
;;;    The new key bindings are useful because adding Shift gives...
;;;;
;;;; - M-S-<up/down/left/right>...
;;;;   Navigate around and update the other window to show the
;;;;   newly-selected item.
;;;; 
;;;; Does not show the contents of the current directory (.) because the
;;;; underlying functionality (dired-display-file) doesn't.
;;;; 
;;;; I suggest the following:
;;;; - Have "A" in your dired-listing-switches so that . and ..
;;;;   are not displayed.
;;;; - Use this with two windows side by side to make best use of
;;;;   vertical space.
;;;;
;;;; You may find something that suits you better by reading
;;;; http://www.emacswiki.org/emacs/DiredMode.

;;;; ___________________________________________________________________________

(require 'cl)

(setq dired-auto-revert-buffer t)

(defvar nomis-dired-do-display-file t)

(defun* nomis-dired-find-file-if-dir-helper (&key (beep-if-not-dir t))
  (if (file-directory-p (dired-get-filename nil t))
      (dired-find-file)
    (when beep-if-not-dir
      (beep))))

(defun nomis-dired-find-file-if-dir ()
  "Nomis Dired Explorer:
If selected entry is a directory go into it."
  (interactive)
  (nomis-dired-find-file-if-dir-helper :beep-if-not-dir t))

(defun nomis-dired-previous-line (arg)
  "Nomis Dired Explorer:
Move up lines and display file in other window."
  (interactive "p")
  (dired-previous-line arg)
  (dired-display-file))

(defun nomis-dired-next-line (arg)
  "Nomis Dired Explorer:
Move down lines and display file in other window."
  (interactive "p")
  (dired-next-line arg)
  (dired-display-file))

(defun nomis-dired-down-directory ()
  "Nomis Dired Explorer:
Go into selected directory and display its contents in other window."
  (interactive)
  (nomis-dired-find-file-if-dir-helper :beep-if-not-dir nil)
  (dired-display-file))

(defun nomis-dired-up-directory ()
  "Nomis Dired Explorer:
Go up a directory and display its contents in other window."
  (interactive)
  (dired-up-directory)
  (dired-display-file))

(add-hook
 'dired-mode-hook
 (lambda ()
   (define-key dired-mode-map (kbd "M-<RET>") 'dired-display-file)
   
   (define-key dired-mode-map (kbd "M-<up>") 'dired-previous-line)
   (define-key dired-mode-map (kbd "M-<down>") 'dired-next-line)
   (define-key dired-mode-map (kbd "M-<left>") 'dired-up-directory)
   (define-key dired-mode-map (kbd "M-<right>") 'nomis-dired-find-file-if-dir)
   
   (define-key dired-mode-map (kbd "M-S-<up>") 'nomis-dired-previous-line)
   (define-key dired-mode-map (kbd "M-S-<down>") 'nomis-dired-next-line)
   (define-key dired-mode-map (kbd "M-S-<left>") 'nomis-dired-up-directory)
   (define-key dired-mode-map (kbd "M-S-<right>") 'nomis-dired-down-directory)))

;;;; ___________________________________________________________________________

(provide 'nomis-dired-explorer)
