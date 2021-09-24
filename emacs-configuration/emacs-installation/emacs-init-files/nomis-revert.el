;;;; Init stuff -- nomis-revert --  -*- lexical-binding: t -*-

;;;; TODO: Consider adding `nomis/with-before-and-after`.

(require 'dash)
(require 's)
(require 'nomis-msg)

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

(defun nomis/with-cleanup-on-non-local-exit/fun (f cleanup-f)
  (let* ((non-local-exit? t))
    (unwind-protect
        (prog1 (funcall f)
          (setq non-local-exit? nil))
      (when non-local-exit?
        (funcall cleanup-f)))))

(defmacro nomis/with-cleanup-on-non-local-exit (bodyform &rest cleanup-forms)
  (declare (indent 1))
  `(nomis/with-cleanup-on-non-local-exit/fun (lambda () ,bodyform)
                                             (lambda () ,@cleanup-forms)))

(defun nomis/y-or-n-p-reporting-non-local-exit (prompt)
  (nomis/with-cleanup-on-non-local-exit
      (y-or-n-p prompt)
    (nomis/message-no-disp "==== Quitting")))

(defun nomis/buffers->string-of-names (buffers)
  (s-join " "
          (-map #'buffer-file-name buffers)))

;;;; ___________________________________________________________________________

(defun nomis/magit-refresh ()
  "A wrapper for `magit-refresh` that we can add our revert
functionality to. We can't add our revert functionality to
`magit-refresh` because lots of things call it, so that would
mean we'd get polite-set-up-behaviour AFTER a command has
been run."
  (interactive)
  (magit-refresh))

;;;; ___________________________________________________________________________
;;;; ---- Reverting buffers ----

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
  ;; Copy-and-hack of http://www.emacswiki.org/emacs/RevertBuffer
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
  (when (nomis/y-or-n-p-reporting-non-local-exit
         "Really revert modified buffers? You will lose stuff.")
    (nomis/revert-all-buffers 'buffer-modified-p
                              inhibit-message?)))

;;;; ___________________________________________________________________________
;;;; ---- Polite reverting ----

;;;; The approach here is a hack that uses a global var and doesn't work if the
;;;; user has more than one ongoing Magit operation. An example of when
;;;; reverting won't happen: user is creating commits in different repos at the
;;;; same time (so with two different COMMIT_EDITMSG buffers on the go).

;;;; We have auto-revert turned off, and we only revert when we are sure it
;;;; won't clobber anything.

(defun nomis/polite-revert/-make-multiline-string (&rest strings)
  (s-join "\n" strings))

(defconst nomis/polite-revert/-log-string/setup-begin
  (nomis/polite-revert/-make-multiline-string
   ""
   ">>>> ▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼"
   ">>>> polite-revert/setup"))

(defconst nomis/polite-revert/-log-string/setup-end
  (nomis/polite-revert/-make-multiline-string
   "<<<< polite-revert/setup"
   "<<<< ▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲"
   ""))

(defconst nomis/polite-revert/-log-string/revert-begin
  (nomis/polite-revert/-make-multiline-string
   ""
   ">>>> ▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼"
   ">>>> polite-revert/revert"))

(defconst nomis/polite-revert/-log-string/revert-end
  (nomis/polite-revert/-make-multiline-string
   "<<<< polite-revert/revert"
   "<<<< ▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲"
   ""))

(defconst nomis/polite-revert/-log-string/note-begin
  "    ▼▼▼▼▼▼▼▼▼▼▼▼")

(defconst nomis/polite-revert/-log-string/note-end
  "    ▲▲▲▲▲▲▲▲▲▲▲▲")

(defun nomis/polite-revert/-maybe-revert-out-of-sync-buffers
    (&optional force?)
  (nomis/message-no-disp
   ">>>> nomis/polite-revert/-maybe-revert-out-of-sync-buffers")
  (unwind-protect
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
             (revert-buffers? (or force?
                                  (if (not buffers-to-maybe-revert)
                                      nil
                                    (nomis/y-or-n-p-reporting-non-local-exit
                                     (format
                                      "Do you want to revert the following %s buffer(s) that are out-of-sync with their files? %s"
                                      (length buffers-to-maybe-revert)
                                      buffers-to-maybe-revert))))))
        (when revert-buffers?
          (dolist (b buffers-to-maybe-revert)
            (nomis/message-no-disp "==== ==== Reverting %s" b)
            (nomis/revert-buffer-ignoring-failures b))))
    (nomis/message-no-disp
     "<<<< nomis/polite-revert/-maybe-revert-out-of-sync-buffers")))

(defun nomis/polite-revert/-maybe-revert-out-of-sync-buffers/forced ()
  (nomis/polite-revert/-maybe-revert-out-of-sync-buffers t))

(defvar *nomis/polite-revert/-advised-commands* nil)

(defvar nomis/polite-revert/-revert-buffers? nil) ; we turn this on when we are sure it's OK, and then turn it off again

(defun nomis/polite-revert/-report-status/helper (middle-words)
  (nomis/message-no-disp
   "==== nomis/polite-revert/-revert-buffers? %s %s"
   middle-words
   nomis/polite-revert/-revert-buffers?))

(defun nomis/polite-revert/-report-status/new ()
  (nomis/polite-revert/-report-status/helper "has been set to"))

(defun nomis/polite-revert/-report-status/existing ()
  (nomis/polite-revert/-report-status/helper "="))

(defun nomis/polite-revert/-do-set-up ()
  (let* ((in-current-repo?-fun (nomis/-vc-make/buffer-in-current-repo?-fun))
         (obsolete-unmodified?-fun
          (lambda (b)
            (nomis/-vc-buffer-unmodified-and-out-of-date? in-current-repo?-fun b)))
         (obsolete-unmodified-buffers
          (nomis/find-buffers obsolete-unmodified?-fun))
         (n-obsolete-unmodified-buffers (length obsolete-unmodified-buffers)))
    (if (null obsolete-unmodified-buffers)
        (setq nomis/polite-revert/-revert-buffers? t)
      (progn
        (nomis/message-no-disp "==== this-command = %s" this-command)
        (nomis/message-no-disp "==== advised commands = %s"
                               *nomis/polite-revert/-advised-commands*)
        (nomis/message-no-disp
         "==== There are %s out-of-sync buffer(s) = %s"
         n-obsolete-unmodified-buffers
         (nomis/buffers->string-of-names obsolete-unmodified-buffers))
        (let* ((msg (format "There are %s buffer(s) that are out-of-sync with their files. Are you happy to clobber them? If not, all reverting will be turned off for this operation.\nSee *Messages* buffer for details\n"
                            n-obsolete-unmodified-buffers)))
          (setq nomis/polite-revert/-revert-buffers?
                (nomis/y-or-n-p-reporting-non-local-exit (format "%s" msg))))))
    (nomis/polite-revert/-report-status/new)))

(progn ; nomis/polite-revert/-advice -- treat this as a unit of work

  ;; magit-push
  ;; with-editor-finish

  (defconst nomis/polite-revert/-commands/non-reverting
    '(magit-stage
      magit-unstage))

  (defconst nomis/polite-revert/-commands/reverting/not-requiring-setup
    '())

  (defconst nomis/polite-revert/-commands/reverting/requiring-setup
    (progn
      ;; If already defined, remove the advice that was set up (so that we can
      ;; remove entries from this list in dev).
      (when (boundp 'nomis/polite-revert/-commands/reverting/requiring-setup)
        (dolist (c nomis/polite-revert/-commands/reverting/requiring-setup)
          (advice-remove c 'nomis/polite-revert/-advice)))
      '(nomis/magit-refresh
        magit-commit-amend
        magit-commit-create
        magit-discard
        magit-reset-hard
        magit-reset-soft
        magit-stash-both
        magit-stash-index
        magit-stash-pop)))

  (dolist (c nomis/polite-revert/-commands/reverting/requiring-setup)
    (advice-add
     c
     :around
     (lambda (orig-fun &rest args)
       (cl-flet* ((do-it () (apply orig-fun args)))
         ;; TODO: Move this to a (shared) function that takes #'do-it as
         ;;       an argument.
         (when (null *nomis/polite-revert/-advised-commands*)
           (nomis/message-no-disp "%s"
                                  nomis/polite-revert/-log-string/setup-begin))
         (nomis/message-no-disp ">>>> %s" c)
         (let* ((do-special-stuff?
                 (null *nomis/polite-revert/-advised-commands*)))
           (unwind-protect
               (let* ((*nomis/polite-revert/-advised-commands*
                       (cons c *nomis/polite-revert/-advised-commands*)))
                 (if (not do-special-stuff?)
                     (do-it)
                   (progn
                     (nomis/polite-revert/-do-set-up)
                     (nomis/with-cleanup-on-non-local-exit
                         (do-it)
                       ;; Can get here, for example, if user enters C-g when
                       ;; asked whether to save a file.
                       (nomis/message-no-disp "==== non-local exit")
                       (when nomis/polite-revert/-revert-buffers?
                         (nomis/polite-revert/-maybe-revert-out-of-sync-buffers/forced)
                         (setq nomis/polite-revert/-revert-buffers?
                               nil)
                         (nomis/polite-revert/-report-status/new))))))
             (nomis/message-no-disp "<<<< %s" c)
             (when (null *nomis/polite-revert/-advised-commands*)
               (nomis/message-no-disp "%s"
                                      nomis/polite-revert/-log-string/setup-end))))))
     '((name . nomis/polite-revert/-advice)))))

(defun nomis/polite-revert/auto-revert ()
  (nomis/message-no-disp "%s" nomis/polite-revert/-log-string/revert-begin)
  (nomis/message-no-disp "==== this-command = %s" this-command)
  (nomis/message-no-disp
   "==== nomis/polite-revert/auto-revert *nomis/polite-revert/-advised-commands* = %s"
   *nomis/polite-revert/-advised-commands*)
  (unwind-protect
      (when (null (cdr *nomis/polite-revert/-advised-commands*))
        (nomis/polite-revert/-report-status/existing)
        (if (member this-command
                    nomis/polite-revert/-commands/non-reverting)
            (nomis/message-no-disp "==== Not reverting because this-command (%s) is a member of `nomis/polite-revert/-commands/non-reverting`"
                                   this-command)
          (unless (or ; (null this-command) ; this happens in `magit-auto-revert-buffers` for `TODO What?`
                   (member this-command
                           nomis/polite-revert/-commands/reverting/requiring-setup)
                   (member this-command
                           nomis/polite-revert/-commands/reverting/not-requiring-setup))
            (nomis/message-no-disp "%s" nomis/polite-revert/-log-string/note-begin)
            (nomis/message-no-disp "    Consider adding `%s` to one of the commands lists"
                                   this-command)
            (nomis/message-no-disp "%s" nomis/polite-revert/-log-string/note-end))
          (when nomis/polite-revert/-revert-buffers?
            (nomis/polite-revert/-maybe-revert-out-of-sync-buffers/forced)
            ;; TODO: We are turning off reverting so that if there's something
            ;;       I'm unclear about we don't revert.
            ;;       i.e. I don't know when Magit calls
            ;;       `magit-auto-revert-buffers`.
            (setq nomis/polite-revert/-revert-buffers? nil)
            (nomis/polite-revert/-report-status/new))))
    (nomis/message-no-disp "%s" nomis/polite-revert/-log-string/revert-end)))

;;;; ___________________________________________________________________________

(defun nomis/revert-out-of-sync-buffers-in-repo-getting-user-confirmation ()
  (interactive)
  (nomis/polite-revert/-maybe-revert-out-of-sync-buffers))

;;;; ___________________________________________________________________________

(with-eval-after-load 'magit-status
  (define-key magit-status-mode-map (kbd "g") 'nomis/magit-refresh))

;;;; ___________________________________________________________________________

(provide 'nomis-revert)
