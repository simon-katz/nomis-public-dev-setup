;;;; Init stuff -- Paredit.

(require 's)

(eval-after-load 'paredit

  ;; See:
  ;; - http://www.emacswiki.org/emacs/PareditCheatsheet
  ;; - http://mumble.net/~campbell/emacs/paredit.html

  '(let* ((disable-cmf-cmb? nil))

     ;; Temporary: Turn off the keybindings for `paredit-forward` and
     ;; `paredit-backward` while you are training yourself not to use them.
     (require 'nomis-popup nil t)
     (defun nomis/do-not-do-paredit-forward-or-paredit-backward (arg)
       (interactive "p")
       (nomis/popup/error-message "No! You are training yourself not to use `paredit-forward` and `paredit-backward`! Use C-M-] and C-M-[ instead."))
     (define-key paredit-mode-map
       (kbd "C-M-f")
       (if disable-cmf-cmb?
           'nomis/do-not-do-paredit-forward-or-paredit-backward
         'forward-sexp))
     (define-key paredit-mode-map
       (kbd "C-M-b")
       (if disable-cmf-cmb?
           'nomis/do-not-do-paredit-forward-or-paredit-backward
         'backward-sexp))

     ;; When just before a close-parenthesis, `paredit-forward` (C-M-f) jumps up
     ;; a level. When just after an open-parenthesis, `paredit-backward`
     ;; (C-M-b) behaves similarly. I don't like that. IMO they should beep
     ;; instead. Fortunately `forward-sexp` and `backward-sexp` do The Right
     ;; Thing, so give them key bindings.
     (define-key paredit-mode-map (kbd "C-M-]") 'forward-sexp)
     (define-key paredit-mode-map (kbd "C-M-[") 'backward-sexp)

     ;; On Mac before 10.15.1 Catalina (I think), we can't use C-M-d because
     ;; it's highjacked at a low level by the system for dictionary lookup. So
     ;; use M-d instead. (The default is paredit-foward-kill-word, which we can
     ;; do without.)
     (when (and (equal system-type 'darwin)
                (ignore-errors (version< (-> "sw_vers -productVersion"
                                             shell-command-to-string
                                             s-trim)
                                         "10.15.1")))
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
