;;;; Init stuff -- Remembering desktop using workgroups.

(require 'workgroups)

;; (setq wg-prefix-key (kbd "C-c C-c"))
(workgroups-mode 1)
(wg-load "~/Documents/jsk/development-100/__for-sync/repositories/nomis/jsk-settings/emacs-workgroups-configuration.txt")

(define-key wg-map (kbd "<right>")   'wg-switch-right)
(define-key wg-map (kbd "C-<right>") 'wg-switch-right)
(define-key wg-map (kbd "<left>")    'wg-switch-left)
(define-key wg-map (kbd "C-<left>")  'wg-switch-left)

(setq wg-use-faces nil) ; avoid dim, hard-to-see colours
(setq wg-morph-on nil) ; attempt to stop Emacs crashing while morphing


(when (equal wg-version "0.2.0")

  ;; Pretty print so I can see what changes happen...
  (defun wg-write-sexp-to-file (sexp file)
    "Write the printable representation of SEXP to FILE."
    ;; Hmmm, cl-prettyprint disappeared. Maybe when I upgraded Emacs.
    ;; (with-temp-buffer
    ;;   (let (print-level print-length)
    ;;     (cl-prettyprint sexp) ; was (insert (format "%S" sexp))
    ;;     (write-file file)))
    (with-temp-buffer
      (insert (with-output-to-string (pp sexp))) ; was (insert (format "%S" sexp))
      (write-file file)))

  ;; Use `abbreviate-file-name' (giving use of `~`) so that
  ;; configuration files can be moved between users or machines.
  (defun wg-ewin->window (ewin)
    "Return a new workgroups window from EWIN.
EWIN should be an Emacs window object."
    (with-current-buffer (window-buffer ewin)
      `((type     .   window)
        (edges    .  ,(window-edges ewin))
        (bname    .  ,(buffer-name))
        (fname    .  ,(let ((file-name (buffer-file-name)))
                        (if (null file-name)
                            nil
                          (abbreviate-file-name file-name))))
        (point    .  ,(wg-window-point ewin))
        (mark     .  ,(mark))
        (markx    .  ,mark-active)
        (wstart   .  ,(window-start ewin))
        (hscroll  .  ,(window-hscroll ewin))
        (sbars    .  ,(window-scroll-bars ewin))
        (margins  .  ,(window-margins ewin))
        (fringes  .  ,(window-fringes ewin))
        (selwin   .  ,(eq ewin (selected-window)))
        (mbswin   .  ,(eq ewin minibuffer-scroll-window)))))

  ;; Rename buffer if the name is already in use...
  (defun wg-switch-to-window-buffer (win)
    "Switch to a buffer determined from WIN's fname and bname.
Return the buffer if it was found, nil otherwise."
    (wg-abind win (fname bname)
      (cond ((and fname (file-exists-p fname))
             (find-file fname)
             (rename-buffer bname t) ; jsk: Added the `t` arg here so ok if name already in use   TODO: With uniquify, is this still needed?
             (current-buffer))
            ((wg-awhen (get-buffer bname) (switch-to-buffer it)))
            (t (switch-to-buffer wg-default-buffer) nil)))))

;;;; ___________________________________________________________________________

(provide 'nomis-remember-desktop-using-workgroups)
