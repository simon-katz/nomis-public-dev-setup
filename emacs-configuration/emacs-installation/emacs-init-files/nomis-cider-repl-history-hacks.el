;;;; Init stuff -- CIDER REPL history hacks --  -*- lexical-binding: t -*-

;;;; ___________________________________________________________________________
;;;; Hack `cider-repl--history-replace` so that if we go beyond the bounds of
;;;; history, the input is cleared. See `:nomis-hack` below.

;;;; :possible-open-source-contribution `cider-repl--history-replace`

(cond
 ((member (pkg-info-version-info 'cider)
          '("1.2.0snapshot (package: 20211105.708)"
            "1.3.0 (package: 20220405.1216)"
            "1.5.0 (package: 20220830.500)"
            "1.7.0 (package: 20230518.550)"
            "20250217.1433"
            "20250430.722"))

  (with-eval-after-load 'cider-repl
    ;; The original is in `cider-repl`.
    (defun cider-repl--history-replace (direction &optional regexp)
      "Replace the current input with the next line in DIRECTION.
DIRECTION is 'forward' or 'backward' (in the history list).
If REGEXP is non-nil, only lines matching REGEXP are considered."
      (setq cider-repl-history-pattern regexp)
      (let* ((min-pos -1)
             (max-pos (length cider-repl-input-history))
             (pos0 (cond ((cider-history-search-in-progress-p)
                          cider-repl-input-history-position)
                         (t min-pos)))
             (pos (cider-repl--position-in-history pos0 direction (or regexp "")))
             (msg nil))
        (cond ((and (< min-pos pos) (< pos max-pos))
               (cider-repl--replace-input (nth pos cider-repl-input-history))
               (setq msg (format "History item: %d" pos)))
              ((not cider-repl-wrap-history)
               (cider-repl--replace-input "") ; :nomis-hack
               (setq msg (cond ((= pos min-pos) "End of history")
                               ((= pos max-pos) "Beginning of history"))))
              (cider-repl-wrap-history
               (setq pos (if (= pos min-pos) max-pos min-pos))
               (setq msg "Wrapped history")))
        (when (or (<= pos min-pos) (<= max-pos pos))
          (when regexp
            (setq msg (concat msg "; no matching item"))))
        (message "%s%s" msg (cond ((not regexp) "")
                                  (t (format "; current regexp: %s" regexp))))
        (setq cider-repl-input-history-position pos)
        (setq this-command 'cider-repl--history-replace)))))

 (t
  (message-box
   "You need to fix `cider-repl--history-replace` for this version of CIDER.")))

;;;; ___________________________________________________________________________
;;;; ---- nomis/-write-cider-repl-history-file-immediately ----

;;;; :possible-open-source-contribution `nomis/-write-cider-repl-history-file-immediately`
;;;; - Need to include (some of) the stuff below this too.

;;;; CIDER REPL history is broken: when quitting, Emacs history is not saved
;;;; properly. CIDER does...
;;;;
;;;;   `(add-hook 'kill-emacs-hook #'cider-repl-history-just-save)`
;;;;
;;;; ...but that's no good. `cider-repl-history-just-save` needs to run with
;;;; a REPL buffer current, because history is stored in a buffer-local variable
;;;; (`cider-repl-input-history`).
;;;;
;;;; If there are multiple REPLs the save function should be called for each
;;;; active REPL, but it isn't.
;;;;
;;;; We get around these problems by updating history files as and when new
;;;; commands are entered.
;;;;
;;;; As a bonus, we have separate histories for CLJ and CLJS.

(defvar nomis/-write-cider-repl-history-file-immediately
  ;; The purpose of this is to have a thing that we can refer to and can find
  ;; with `M-.`.
  "Without a value here, `M-.` to find this definition doesn't work.")

(defconst nomis/-cider-repl-history-filename-clj  ".cider-repl-history-clj")
(defconst nomis/-cider-repl-history-filename-cljs ".cider-repl-history-cljs")

(defvar nomis/cider-use-centralised-repl-history-location? t
  "For now, at least, set to `nil` using dir-locals.")

(defvar-local nomis/-cider-repl-history-loaded? nil)
(defvar-local nomis/-cider-repl-history-warning-issued? nil)

(cond
 ((member (pkg-info-version-info 'cider)
          '("1.2.0snapshot (package: 20211105.708)"
            "1.3.0 (package: 20220405.1216)"
            "1.5.0 (package: 20220830.500)"
            "1.7.0 (package: 20230518.550)"
            "20250217.1433"
            "20250430.722"))

  (defun nomis/-cider-repl-history-maybe-warn ()
    (when (and cider-repl-history-file
               (not nomis/-cider-repl-history-warning-issued?))
      (setq nomis/-cider-repl-history-warning-issued? t)
      (let* ((msg "You have `cider-repl-history-file` defined, so I will use standard (broken) CIDER functionality."))
        (nomis/msg/grab-user-attention/high)
        (message "%s" msg)
        (message-box "%s" msg))))

  (add-hook 'cider-repl-mode-hook
            'nomis/-cider-repl-history-maybe-warn)

  (defun nomis/-cider-repl-history-file* ()
    (let* ((repl-type (cider-repl-type (current-buffer))))
      (cl-case repl-type
        (clj  nomis/-cider-repl-history-filename-clj)
        (cljs nomis/-cider-repl-history-filename-cljs))))

  (defun nomis/-cider-hacked-project-root (project-root)
    (let* ((prefix "/Users/simonkatz/development-100/repositories/"))
      (when (s-starts-with? prefix project-root)
        (s-chop-prefix prefix project-root))))

  (defun nomis/-cider-repl-history-file ()
    (let* ((project-root (expand-file-name default-directory)) ; We are in the REPL buffer, so this is what we want, even for repos that have multiple projects.
           (hacked-project-root (nomis/-cider-hacked-project-root project-root))
           (history-filename (nomis/-cider-repl-history-file*)))
      (if (not (and nomis/cider-use-centralised-repl-history-location?
                    hacked-project-root))
          (progn
            (let ((inhibit-message t))
              (message "nomis/-cider-repl-history-file -- Using project dir for history file"))
            history-filename)
        (let* ((centralised-dir (concat "/Users/simonkatz/jsk-settings/cider-repl-history-files/"
                                        hacked-project-root))
               (centralised-filepath (concat centralised-dir
                                             (s-chop-prefix "."
                                                            ;; better name, and
                                                            ;; avoid global git
                                                            ;; ignore
                                                            history-filename))))
          (if (file-exists-p centralised-filepath)
              (if (and history-filename
                       (file-exists-p history-filename))
                  (warn "nomis/-cider-repl-history-file -- Ignoring %s because centralised file exists"
                        (concat project-root history-filename))
                (let ((inhibit-message t))
                  (message "nomis/-cider-repl-history-file -- Using existing file in centralised location")))
            (if (and history-filename
                     (file-exists-p history-filename))
                (progn
                  (let ((inhibit-message t))
                    (message "nomis/-cider-repl-history-file -- Moving history file from %s to %s"
                             history-filename
                             centralised-filepath))
                  (make-directory centralised-dir t)
                  (rename-file history-filename centralised-filepath))
              (progn
                (let ((inhibit-message t))
                  (message "nomis/-cider-repl-history-file -- Creating new history file at %s"
                           centralised-filepath))
                (make-directory centralised-dir t))))
          centralised-filepath))))

  (defun nomis/-cider-repl-history-maybe-load ()
    (unless nomis/-cider-repl-history-loaded?
      (setq nomis/-cider-repl-history-loaded? t)
      (let* ((filename (nomis/-cider-repl-history-file)))
        (when filename
          (when cider-repl-history-file
            (message "cider-repl-history-file is unexpectedly non-nil: %s"
                     cider-repl-history-file))
          (cider-repl-history-load filename)
          (when nomis/-write-cider-repl-history-file-immediately
            ;; CIDER sets this, but we don't want it.
            (setq cider-repl-history-file nil))))))

  (defun nomis/cider-repl--history-write-most-recent-item (filename)
    "Write history to FILENAME.
Currently coding system for writing the contents is hardwired to
utf-8-unix."
    ;; A hacked copy of `cider-repl--history-write` that appends the most recent
    ;; history item to the file.
    (let* ((mhist (cons (first cider-repl-input-history)
                        (cider-repl--history-read filename)))
           ;; newest items are at the beginning of the list, thus 0
           (hist (cl-subseq mhist 0 (min (length mhist) cider-repl-history-size))))
      (unless (file-writable-p filename)
        (error (format "History file not writable: %s" filename)))
      (let ((print-length nil) (print-level nil))
        (with-temp-file filename
          (insert ";; -*- coding: utf-8-unix -*-\n")
          (insert ";; Automatically written history of CIDER REPL session\n")
          (insert ";; Edit at your own risk\n\n")
          (insert "(\n")
          (cl-loop for s in hist
                   do (insert " ")
                   do (prin1 (substring-no-properties s) (current-buffer))
                   do (insert "\n"))
          (insert ")\n")))))

  (defun nomis/-cider-repl-history-maybe-write-most-recent-item ()
    (let* ((filename (nomis/-cider-repl-history-file)))
      (when filename
        (nomis/cider-repl--history-write-most-recent-item filename))))

  ;; Ideally we would load history immediately after the REPL is initialised,
  ;; but that would have to be done after CLJS REPLs become CLJS REPLs
  ;; (initially they are CLJ REPLs), and we can't work out where that change
  ;; happens. So instead we load history just before it is needed.
  (dolist (command '(cider-repl-previous-input
                     cider-repl-next-input
                     cider-repl-forward-input
                     cider-repl-backward-input
                     cider-repl-previous-matching-input
                     cider-repl-next-matching-input
                     cider-repl--add-to-input-history))
    (advice-add
     command
     :before
     (lambda (&rest _args)
       (when (null cider-repl-history-file)
         (nomis/-cider-repl-history-maybe-load)))
     '((name . nomis/-write-cider-repl-history-file-immediately/load))))

  (advice-add
   'cider-repl--add-to-input-history
   :after
   (lambda (&rest _args)
     (when (null cider-repl-history-file)
       (nomis/-cider-repl-history-maybe-write-most-recent-item)))
   '((name . nomis/-write-cider-repl-history-file-immediately/write))))

 (t
  ;; Use CIDER built-in behaviour.
  (setq cider-repl-history-file nomis/-cider-repl-history-filename-clj)
  (message-box
   "You need to fix `nomis/-write-cider-repl-history-file-immediately` for this version of CIDER.")))

(cond
 ((member (pkg-info-version-info 'cider)
          '("20250217.1433"
            "20250430.722"))
  ;; CIDER does do this but we need it earlier, otherwise /eg/ CLJ and CLJS
  ;; histories can get muddled.
  (make-variable-buffer-local 'cider-repl-input-history))

 (t
  (message-box
   "You need to fix making `cider-repl-input-history` buffer local for this version of CIDER.")))

;;;; ___________________________________________________________________________
;;;; ---- nomis/hack-check-cider-repl-history-file ----

(advice-add
 'cider-repl-history-just-save ; this can be called when `cider-repl-history-file` is nil
 :around
 (lambda (orig-fun &rest args)
   (when cider-repl-history-file
     (apply orig-fun args)))
 '((name . nomis/hack-check-cider-repl-history-file)))

;;;; ___________________________________________________________________________
;;;; ---- nomis/-note-random-history-files ----

;; This shouldn't be needed now that we are dealing with CIDER history
;; ourselves in `nomis/-write-cider-repl-history-file-immediately`.
(cond
 ((or (member (nomis/cider-version)
              '("CIDER 0.26.1 (Nesebar)"))
      (member (pkg-info-version-info 'cider)
              '("1.2.0snapshot (package: 20210909.1011)"
                "1.2.0snapshot (package: 20210929.1032)"
                "1.2.0snapshot (package: 20211105.708)"
                "1.3.0 (package: 20220405.1216)"
                "1.5.0 (package: 20220830.500)"
                "1.7.0 (package: 20230518.550)"

                ;; Don't really need to add new versions here if we are right
                ;; that this problem has gone away now that we are using
                ;; `nomis/-write-cider-repl-history-file-immediately`, but
                ;; there's no harm in adding them.
                )))
  (advice-add
   'cider-repl-history-just-save
   :after
   (lambda ()
     (message-box "`cider-repl-history-just-save` was unexpectedly called. This shouldn't happen now that we are using `nomis/-write-cider-repl-history-file-immediately`.")
     (let* ((repl-type (cider-repl-type (current-buffer))))
       (when (null repl-type)
         ;; Current buffer is not a REPL buffer.
         (message-box "I think this is a situation where we have created a random CIDER history file. current-buffer = %s"
                      (current-buffer)))))
   '((name . nomis/-note-random-history-files))))

 ((member (pkg-info-version-info 'cider)
          '("20250217.1433"
            ;; With CIDER "20250217.1433", this is called when quitting a REPL
            ;; and when quitting Emacs even when `cider-repl-history-file` is
            ;; nil.
            "20250430.722"))
  ;; Do nothing.
  )

 (t
  (message-box
   "You need to fix `nomis/-note-random-history-files` for this version of CIDER.")))

;; (advice-remove 'cider-repl-history-just-save 'nomis/-note-random-history-files)

;;;; ___________________________________________________________________________

(provide 'nomis-cider-repl-history-hacks)
