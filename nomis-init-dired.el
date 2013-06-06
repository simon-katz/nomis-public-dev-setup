;;;; Init stuff -- Dired.

;;;; ___________________________________________________________________________
;;;; ---- Misc ----

(setq dired-auto-revert-buffer t)

(setq dired-listing-switches "-Ao")

;;;; ___________________________________________________________________________
;;;; ---- "Explorer"-like behaviour ----
;;;; Navigate in one window; display contents in other window.
;;;; I suggest having "A" in your dired-listing-switches so that . and ..
;;;; are not displayed.
;;;; Does not show the contents of the current directory (.) because the
;;;; underlying functionality (dired-display-file) doesn't.

(defvar nomis-dired-do-display-file t)

(defun nomis-toggle-dired-do-display-file ()
  "Nomis Explorer-like dired:
Toggle whether file/directories are displayed (in other window) when navigating."
  (interactive)
  (setq nomis-dired-do-display-file (not nomis-dired-do-display-file)))

(defun nomis-dired-maybe-display-file ()
  (when nomis-dired-do-display-file
    (dired-display-file)))

(defun nomis-dired-previous-line (arg)
  "Nomis Explorer-like dired:
Move up lines and maybe display file in other window."
  (interactive "p")
  (dired-previous-line arg)
  (nomis-dired-maybe-display-file))

(defun nomis-dired-next-line (arg)
  "Nomis Explorer-like dired:
Move down lines and maybe display file in other window."
  (interactive "p")
  (dired-next-line arg)
  (nomis-dired-maybe-display-file))

(defun nomis-dired-into ()
 "Nomis Explorer-like dired:
Go into selected directory and maybe display its contents in other window."
  (interactive)
  (when (file-directory-p (dired-get-filename nil t))
    (dired-find-file))
  (nomis-dired-maybe-display-file))

(defun nomis-dired-back ()
  "Nomis Explorer-like dired:
Go up a directory and maybe display its contents in other window."
  (interactive)
  (dired-up-directory)
  (nomis-dired-maybe-display-file))

(add-hook
 'dired-mode-hook
 (lambda ()
   (define-key dired-mode-map (kbd "M-<RET>") 'dired-display-file)
   
   (define-key dired-mode-map (kbd "M-<up>") 'nomis-dired-previous-line)
   (define-key dired-mode-map (kbd "M-<down>") 'nomis-dired-next-line)
   (define-key dired-mode-map (kbd "M-<left>") 'nomis-dired-back)
   (define-key dired-mode-map (kbd "M-<right>") 'nomis-dired-into)))
