;;;; ___________________________________________________________________________
;;;; ---- "Explorer"-like behaviour for dired ----
;;;; Navigate in one window; display contents in other window.
;;;; Does not show the contents of the current directory (.) because the
;;;; underlying functionality (dired-display-file) doesn't.
;;;; I suggest having "A" in your dired-listing-switches so that . and ..
;;;; are not displayed.

(defvar nomis-dired-do-display-file t)

(defun nomis-dired-find-file-if-dir ()
 "Nomis Explorer-like dired:
If selected entry is a directory go into it."
  (interactive)
  (when (file-directory-p (dired-get-filename nil t))
    (dired-find-file)))

(defun nomis-dired-previous-line (arg)
  "Nomis Explorer-like dired:
Move up lines and maybe display file in other window."
  (interactive "p")
  (dired-previous-line arg)
  (dired-display-file))

(defun nomis-dired-next-line (arg)
  "Nomis Explorer-like dired:
Move down lines and maybe display file in other window."
  (interactive "p")
  (dired-next-line arg)
  (dired-display-file))

(defun nomis-dired-into ()
  "Nomis Explorer-like dired:
Go into selected directory and maybe display its contents in other window."
  (interactive)
  (nomis-dired-find-file-if-dir)
  (dired-display-file))

(defun nomis-dired-up-directory ()
  "Nomis Explorer-like dired:
Go up a directory and maybe display its contents in other window."
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
   (define-key dired-mode-map (kbd "M-S-<right>") 'nomis-dired-into)))

(provide 'nomis-dired-explorer)
