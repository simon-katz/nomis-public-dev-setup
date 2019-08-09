;;;; Init stuff -- Paredit.

(eval-after-load 'paredit

  ;; See:
  ;; - http://www.emacswiki.org/emacs/PareditCheatsheet
  ;; - http://mumble.net/~campbell/emacs/paredit.html

  '(progn

     ;; Stop C-M-f and C-M-b jumping up a level when they should
     ;; (IMHO) beep, by using forward-sexp and backward-sexp rather
     ;; than the (IMHO) broken paredit versions.
     (define-key paredit-mode-map (kbd "C-M-f") 'forward-sexp)
     (define-key paredit-mode-map (kbd "C-M-b") 'backward-sexp)

     ;; Easier-to-use key bindings for forward-sexp and backward-sexp.
     (define-key paredit-mode-map (kbd "M-]") 'forward-sexp)
     (define-key paredit-mode-map (kbd "M-[") 'backward-sexp)
     (define-key paredit-mode-map (kbd "M-S-<right>") 'forward-sexp)
     (define-key paredit-mode-map (kbd "M-S-<left>") 'backward-sexp)

     ;; On Mac, I can't use C-M-d because it's highjacked at a low level
     ;; by the system for dictionary lookup.
     ;; Use M-d instead.  (The default is paredit-foward-kill-word, which
     ;; I can do without.)
     (when (equal system-type 'darwin)
       (define-key paredit-mode-map (kbd "M-d")
         'paredit-forward-down))

     ;; Weird bug: point was sometimes moving to beginning-of-defun when I
     ;; was deleting a character.
     (when (eql paredit-version 24)
       (advice-add 'paredit-current-parse-state
                   :around
                   (lambda (orig-fun)
                     (save-excursion (funcall orig-fun)))
                   '((name . save-excursion))))))

;;;; ___________________________________________________________________________

(provide 'nomis-paredit)
