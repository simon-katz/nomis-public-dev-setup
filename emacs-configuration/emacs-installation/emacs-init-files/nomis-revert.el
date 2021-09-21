;;;; Init stuff -- nomis-revert --  -*- lexical-binding: t -*-

(require 'dash)
(require 'nomis-msg)

;;;; ___________________________________________________________________________

(defconst nomis/-magit-reverting-log-begin
  "▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼")

(defconst nomis/-magit-reverting-log-end
  "▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲")

(defconst nomis/-magit-note-begin
  "    ▼▼▼▼▼▼▼▼▼▼▼▼")

(defconst nomis/-magit-note-end
  "    ▲▲▲▲▲▲▲▲▲▲▲▲")

;;;; ___________________________________________________________________________
;;;; ---- Various utilities ----

(defun nomis/vc-buffer-in-current-repo? (b)
  (case 2
    (1 (magit-auto-revert-repository-buffer-p b))
    (2 (s-starts-with? (nomis/dirtree/vc-root-dir)
                       (buffer-file-name b)))))

(defun nomis/-vc-make/buffer-in-current-repo?-fun ()
  (let* ((this-buffer (current-buffer)))
    (lambda (b)
      (with-current-buffer this-buffer
        (nomis/vc-buffer-in-current-repo? b)))))

(defun nomis/-vc-buffer-unmodified? (in-current-repo?-fun b)
  (and (buffer-file-name b)
       (not (buffer-modified-p b))
       (funcall in-current-repo?-fun b)))

(defun nomis/-vc-buffer-unmodified-and-up-to-date? (in-current-repo?-fun b)
  (and (nomis/-vc-buffer-unmodified? in-current-repo?-fun b)
       (verify-visited-file-modtime b)))

(defun nomis/-vc-buffer-unmodified-and-out-of-date? (in-current-repo?-fun b)
  (and (nomis/-vc-buffer-unmodified? in-current-repo?-fun b)
       (not (verify-visited-file-modtime b))))

(defun nomis/find-buffers (&rest buffer-predicates)
  (let* ((buffers '()))
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (when (-every? (lambda (pred) (funcall pred buf))
                       buffer-predicates)
          (push buf buffers))))
    buffers))

;;;; ___________________________________________________________________________
;;;; Reverting buffers.
;;;; Copy-and-hack of http://www.emacswiki.org/emacs/RevertBuffer.

(defun nomis/revert-buffer/simple ()
  (revert-buffer t t t))

(defun nomis/revert-buffer-ignoring-failures (&optional b)
  (let* ((b (or b (current-buffer))))
    (with-current-buffer b
      (condition-case e
          (nomis/revert-buffer/simple)
        (error
         (nomis/message-no-disp "==== Failed to revert %s" b))))))

(defun nomis/revert-all-buffers (buffer-predicate inhibit-message?)
  "Revert all buffers that satisfy `buffer-predicate`.
For each buffer, the predicate is run with the buffer as the
current buffer. Unless `inhibit-message?` is non-nil, emit
a message when finished. The message includes the names of any
buffers that could not be reverted."
  (let ((failures '()))
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (when (and (buffer-file-name) (funcall buffer-predicate buf))
          (condition-case e
              (nomis/revert-buffer/simple)
            (error
             (push buf failures)
             (nomis/message-no-disp "%s" e)
             (beep))))))
    (let* ((inhibit-message inhibit-message?))
      (message "Refreshed open files. %s"
               (if failures
                   (s-join " " (cons "Failures: "
                                     (-map #'buffer-file-name failures)))
                 "")))))

(defun nomis/revert-all-unmodified-buffers (inhibit-message?)
  "Refreshes all open unmodified buffers from their files."
  (interactive "P")
  (nomis/revert-all-buffers (lambda (b) (not (buffer-modified-p b)))
                            inhibit-message?))

(defun nomis/revert-all-modified-buffers (inhibit-message?)
  "Refreshes all open modified buffers from their files."
  ;; Copied from http://www.emacswiki.org/emacs/RevertBuffer, and renamed.
  (interactive "P")
  (when (y-or-n-p "Really revert modified buffers? You will lose stuff.")
    (nomis/revert-all-buffers 'buffer-modified-p
                              inhibit-message?)))

;;;; ___________________________________________________________________________
;;;; ---- Fallback reverting ----

(defun nomis/magit-fallback-revert/maybe-revert-out-of-sync-buffers ()
  (nomis/message-no-disp
   "==== nomis/magit-fallback-revert/maybe-revert-out-of-sync-buffers")
  (let* ((in-current-repo?-fun (nomis/-vc-make/buffer-in-current-repo?-fun))
         (revert-buffer?-fun (lambda (b)
                               (and (buffer-file-name b)
                                    ;; The user will already have been
                                    ;; given the opportunity to save
                                    ;; modified buffers, so we silently
                                    ;; don't revert modified buffers.
                                    (not (buffer-modified-p b))
                                    (funcall in-current-repo?-fun b)
                                    (not (verify-visited-file-modtime b)))))
         (buffers-to-maybe-revert (nomis/find-buffers revert-buffer?-fun))
         (revert-buffers? (if (not buffers-to-maybe-revert)
                              nil
                            (y-or-n-p (format
                                       "==== FALLBACK: (for %s): Do you want to revert the following buffers that are out-of-sync with their files? %s"
                                       this-command
                                       buffers-to-maybe-revert)))))
    (when revert-buffers?
      (dolist (b buffers-to-maybe-revert)
        (nomis/message-no-disp "==== FALLBACK: Reverting %s" b)
        (nomis/revert-buffer-ignoring-failures b)))))

;;;; ___________________________________________________________________________
;;;; ---- Clever reverting ----

(defvar nomis/-magit-clever-revert/set-up-and-waiting? nil)
(defvar nomis/-magit-clever-revert/obsolete-buffers-to-not-revert '())
(defvar *nomis/-magit-clever-revert/advised-commands* nil)

(defun nomis/-magit-clever-revert/user-happy-with-any-modified-buffers? ()
  (let* ((in-current-repo?-fun (nomis/-vc-make/buffer-in-current-repo?-fun))
         (unsaved-buffers (nomis/find-buffers
                           (lambda (b)
                             (and (buffer-file-name b)
                                  (buffer-modified-p b)
                                  (funcall in-current-repo?-fun b)))))
         (prompt-1 "There are unsaved buffers. If you continue you will be asked many times whether you want to save them. The questions might sometimes be obliterated by other messages. This is probably a bad idea. Do you want to continue?")
         (prompt-2 "Are you absolutely sure? I won't ask again."))
    (if unsaved-buffers
        (and (y-or-n-p prompt-1)
             (y-or-n-p prompt-2))
      t)))

(defun nomis/-magit-clever-revert/do-set-up/part-2 ()
  ;; Set up later possible non-reverting of obsolete unmodified buffers.
  (let* ((in-current-repo?-fun (nomis/-vc-make/buffer-in-current-repo?-fun))
         (up-to-date-and-unmodified?-fun
          (lambda (b)
            (nomis/-vc-buffer-unmodified-and-up-to-date? in-current-repo?-fun b)))
         (obsolete-unmodified?-fun
          (lambda (b)
            (nomis/-vc-buffer-unmodified-and-out-of-date? in-current-repo?-fun b)))
         (up-to-date-unmodified-buffers
          (nomis/find-buffers up-to-date-and-unmodified?-fun))
         (obsolete-unmodified-buffers
          (nomis/find-buffers obsolete-unmodified?-fun)))
    (setq nomis/-magit-clever-revert/set-up-and-waiting? t)
    (setq nomis/-magit-clever-revert/obsolete-buffers-to-not-revert
          (when (and obsolete-unmodified-buffers
                     (let* ((msg (format
                                  "CLEVER: (for %s, advised command = %s): Do you want to revert the following buffers that are out-of-sync with their files? %s"
                                  this-command
                                  *nomis/-magit-clever-revert/advised-commands*
                                  (s-join " " (-map #'buffer-file-name
                                                    obsolete-unmodified-buffers)))))
                       (not (y-or-n-p msg))))
            obsolete-unmodified-buffers))))

(defun nomis/-magit-clever-revert/do-set-up ()
  (let* ((happy?
          (nomis/-magit-clever-revert/user-happy-with-any-modified-buffers?)))
    (when happy?
      (nomis/-magit-clever-revert/do-set-up/part-2)
      t)))

(progn ; nomis/-magit-clever-revert/advice -- treat this as a unit of work

  (defconst nomis/-magit-clever-revert/commands
    (progn
      ;; If already defined, remove the advice that was set up (so that we can
      ;; remove entries from this list in dev).
      (when (boundp 'nomis/-magit-clever-revert/commands)
        (dolist (c nomis/-magit-clever-revert/commands)
          (advice-remove c 'nomis/-magit-clever-revert/advice)))
      '(nomis/magit-refresh
        magit-commit-amend
        magit-commit-create
        magit-discard)))

  (dolist (c nomis/-magit-clever-revert/commands)
    (advice-add c
                :around
                (lambda (orig-fun &rest args)
                  (when (null *nomis/-magit-clever-revert/advised-commands*)
                    (nomis/message-no-disp "%s"
                                           nomis/-magit-reverting-log-begin))
                  (nomis/message-no-disp ">>>> %s" c)
                  (let* ((do-special-stuff? (null *nomis/-magit-clever-revert/advised-commands*)))
                    (prog1
                        (let* ((*nomis/-magit-clever-revert/advised-commands*
                                (cons c *nomis/-magit-clever-revert/advised-commands*)))
                          (when (or (not do-special-stuff?)
                                    (nomis/-magit-clever-revert/do-set-up))
                            (apply orig-fun args)))
                      (nomis/message-no-disp "<<<< %s" c)
                      (when (null *nomis/-magit-clever-revert/advised-commands*)
                        (nomis/message-no-disp "%s"
                                               nomis/-magit-reverting-log-end)))))
                '((name . nomis/-magit-clever-revert/advice)))))

(defun nomis/-magit-clever-revert/do-reverts ()
  (nomis/message-no-disp "==== nomis/-magit-clever-revert/do-reverts")
  (let* ((in-current-repo?-fun (nomis/-vc-make/buffer-in-current-repo?-fun))
         (revert-buffer?-fun (lambda (b)
                               (and (buffer-file-name b)
                                    ;; The user will already have been
                                    ;; given the opportunity to save
                                    ;; modified buffers, so we silently
                                    ;; don't revert modified buffers.
                                    (not (buffer-modified-p b))
                                    (not (member b nomis/-magit-clever-revert/obsolete-buffers-to-not-revert))
                                    (funcall in-current-repo?-fun b)
                                    (not (verify-visited-file-modtime b)))))
         (buffers-to-revert (nomis/find-buffers revert-buffer?-fun)))
    (dolist (b buffers-to-revert)
      (nomis/message-no-disp "==== POST: Reverting %s" b)
      (nomis/revert-buffer-ignoring-failures b))
    (setq nomis/-magit-clever-revert/set-up-and-waiting? nil)
    (setq nomis/-magit-clever-revert/obsolete-buffers-to-not-revert '())))

;;;; ___________________________________________________________________________
;;;; ---- Choosing how to revert ----

(defconst nomis/-magit-non-reverting-commands
  '(magit-stage
    magit-unstage))

(defun nomis/-magit-maybe-revert ()
  (nomis/message-no-disp
   "==== nomis/-magit-maybe-revert *nomis/-magit-clever-revert/advised-commands* = %s"
   *nomis/-magit-clever-revert/advised-commands*)
  (cond (nomis/-magit-clever-revert/set-up-and-waiting?
         (nomis/-magit-clever-revert/do-reverts))
        ((null *nomis/-magit-clever-revert/advised-commands*)
         (unless (member this-command
                         nomis/-magit-non-reverting-commands)
           (nomis/message-no-disp "%s" nomis/-magit-reverting-log-begin)
           (nomis/message-no-disp "%s" nomis/-magit-note-begin)
           (nomis/message-no-disp "    Consider adding `%s` to `nomis/-magit-clever-revert/commands` or `nomis/-magit-non-reverting-commands`"
                                  this-command)
           (nomis/message-no-disp "%s" nomis/-magit-note-end)
           (nomis/magit-fallback-revert/maybe-revert-out-of-sync-buffers)
           (nomis/message-no-disp "%s" nomis/-magit-reverting-log-end)))))

;;;; ___________________________________________________________________________

(defun nomis/revert-out-of-sync-buffers-in-repo-getting-user-confirmation ()
  (interactive)
  (nomis/magit-fallback-revert/maybe-revert-out-of-sync-buffers))

;;;; ___________________________________________________________________________

(defun nomis/magit-refresh ()
  "A wrapper for `magit-refresh` that we can add our revert
functionality to. We can't add our revert functionality to
`magit-refresh` because lots of things call it, so that would
mean we'd get clever-set-up-behaviour AFTER a command has
been run."
  (interactive)
  (magit-refresh))

(with-eval-after-load 'magit-status
  (define-key magit-status-mode-map (kbd "g") 'nomis/magit-refresh))

;;;; ___________________________________________________________________________

(provide 'nomis-revert)
