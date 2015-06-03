;;;; Init stuff -- CIDER extras

;;## ;;;; TODO: Look at all this in the light of:
;;## ;;;;       - Now have nrepl.el 0.1.8. (Hmmm, no, reverted to 0.1.7 because eldoc
;;## ;;;;         doesn't hapen with 0.1.8.)
;;## ;;;;         See https://groups.google.com/forum/#!topic/nrepl-el/RZTitQyb6mo.
;;## ;;;;       - Did/does it all make sense anyway?

(require 'cider)

;;;; ___________________________________________________________________________
;;;; ---- Wrappers for things in Cider, to isolate dependencies and make ----
;;;; ---- it easier to upgrade Cider.                                    ----

(cond
 ((member (cider-version)
          '("CIDER 0.7.0"))
  (defun nomis-clojure-buffer-ns ()
    (cider-find-ns)))
 ((member (cider-version)
          '("CIDER 0.8.2"))
  (defun nomis-clojure-buffer-ns ()
    (clojure-find-ns)))
 (t
  (message-box
   "You need to fix nomis-clojure-buffer-ns for this version of Cider.")))

(cond
 ((member (cider-version)
          '("CIDER 0.7.0"
            "CIDER 0.8.2"))
  (defun nomis-cider-repl-namespace ()
    (with-current-buffer (cider-current-repl-buffer)
      nrepl-buffer-ns)))
 (t
  (message-box
   "You need to fix `nomis-cider-repl-namespace` for this version of Cider.")))

(cond
 ((member (cider-version)
          '("CIDER 0.7.0"))
  (defun nomis-cider-find-or-create-repl-buffer ()
    (cider-find-or-create-repl-buffer)))
 ((member (cider-version)
          '("CIDER 0.8.2"))
  (defun nomis-cider-find-or-create-repl-buffer ()
    (cider-get-repl-buffer)))
 (t
  (message-box
   "You need to fix `nomis-cider-find-or-create-repl-buffer` for this version of Cider.")))


;;## ;;;; ___________________________________________________________________________
;;## ;;;; ---- Cause focus to go to stacktrace window when popped up -----
;;## 
;;## (cond
;;## 
;;##  ((equal nrepl-current-version "0.1.6")
;;##   (defadvice nrepl-default-err-handler (after select-nrepl-error-buffer ())
;;##     (when (or nrepl-popup-stacktraces
;;##               (not (member (buffer-local-value 'major-mode buffer)
;;##                            '(nrepl-mode clojure-mode))))
;;##       (select-window (get-buffer-window nrepl-error-buffer))))
;;##   (ad-activate 'nrepl-default-err-handler))
;;## 
;;##  ((equal nrepl-current-version "0.1.7")
;;##   ;; Too hard to work out suitable advice, so redefine the function
;;##   ;; with a change.
;;##   (defun nrepl-default-err-handler (buffer ex root-ex session)
;;##     "Make an error handler for BUFFER, EX, ROOT-EX and SESSION."
;;##     ;; TODO: use ex and root-ex as fallback values to display when pst/print-stack-trace-not-found
;;##     (let ((replp (equal 'nrepl-mode (buffer-local-value 'major-mode buffer))))
;;##       (if (or (and nrepl-popup-stacktraces-in-repl replp)
;;##               (and nrepl-popup-stacktraces (not replp)))
;;##           (lexical-let ((nrepl-popup-on-error nrepl-popup-on-error))
;;##             (with-current-buffer buffer
;;##               (nrepl-send-string "(if-let [pst+ (clojure.core/resolve 'clj-stacktrace.repl/pst+)]
;;##                         (pst+ *e) (clojure.stacktrace/print-stack-trace *e))"
;;##                                  (nrepl-make-response-handler
;;##                                   (nrepl-make-popup-buffer nrepl-error-buffer)
;;##                                   nil
;;##                                   (lambda (buffer value)
;;##                                     (nrepl-emit-into-color-buffer buffer value)
;;##                                     (when nrepl-popup-on-error
;;##                                       (nrepl-popup-buffer-display buffer
;;##                                                                   t ; jsk change
;;##                                                                   )))
;;##                                   nil nil) nil session))
;;##             (with-current-buffer nrepl-error-buffer
;;##               (compilation-minor-mode +1))))))))

;;;; ___________________________________________________________________________
;;;; ---- Prompt ----

;;;; I want to enter my input on a fresh line. Nice when you are in a
;;;; namespace that has a long name.

(defvar nomis-cider-repl--hack-prompt-p t)

(defvar nomis-cider-repl--prompt-prefix (concatenate 'string
                                                     (make-string 80 ?\_) "\n"))

(defvar nomis-cider-repl--prompt-suffix "\n")

(cond
 ((member (cider-version)
          '("0.5.0"
            "CIDER 0.6.0alpha (package: 20140210.622)"
            "CIDER 0.6.0"
            "CIDER 0.7.0"))
  (defun cider-repl--insert-prompt (namespace)
    "Insert the prompt (before markers!), taking into account NAMESPACE.
Set point after the prompt.
Return the position of the prompt beginning."
    (goto-char cider-repl-input-start-mark)
    (cider-save-marker cider-repl-output-start
      (cider-save-marker cider-repl-output-end
        (unless (bolp) (insert-before-markers "\n"))
        (let ((prompt-start (point))
              (prompt (let ((original-prompt (format "%s> " namespace)))
                        ;; jsk: Added stuff here
                        (concatenate 'string
                                     (make-string 80 ?\_)
                                     "\n"
                                     original-prompt
                                     "\n"))))
          (cider-propertize-region
              '(face cider-repl-prompt-face read-only t intangible t
                     cider-prompt t
                     rear-nonsticky (cider-prompt read-only face intangible))
            (insert-before-markers prompt))
          (set-marker cider-repl-prompt-start-mark prompt-start)
          prompt-start)))))
 ((and (member (cider-version)
               '("CIDER 0.8.2"))
       (not (boundp 'cider-repl-prompt-function)) ; without my modification
       )
  (defun cider-repl--insert-prompt (namespace)
    "Insert the prompt (before markers!), taking into account NAMESPACE.
Set point after the prompt.
Return the position of the prompt beginning."
    (goto-char cider-repl-input-start-mark)
    (cider-save-marker cider-repl-output-start
      (cider-save-marker cider-repl-output-end
        (unless (bolp) (insert-before-markers "\n"))
        (let ((prompt-start (point))
              (prompt (let ((original-prompt (format "%s> " namespace)))
                        ;; jsk: Added stuff here
                        (concatenate 'string
                                     (make-string 80 ?\_)
                                     "\n"
                                     original-prompt
                                     "\n"))))
          (cider-propertize-region
              '(font-lock-face cider-repl-prompt-face read-only t intangible t
                               cider-repl-prompt t
                               rear-nonsticky (cider-repl-prompt read-only font-lock-face intangible))
            (insert-before-markers prompt))
          (set-marker cider-repl-prompt-start-mark prompt-start)
          prompt-start)))))
 ((boundp 'cider-repl-prompt-function)
  (setq cider-repl-prompt-function
        (lambda (namespace)
          (cl-labels ((do-it () (funcall 'cider-repl-default-prompt namespace)))
            (if nomis-cider-repl--hack-prompt-p
                (concatenate 'string
                             nomis-cider-repl--prompt-prefix
                             (do-it)
                             nomis-cider-repl--prompt-suffix)
              (do-it))))))
 (t
  (message-box
   "You need to fix your Cider prompt stuff for this version of Cider.")))

;;## ;;;; ___________________________________________________________________________
;;## ;;;; ---- Pretty printing of results ----
;;## 
;;## ;;;; Pretty printing of results is broken, because:
;;## ;;;; - It returns (and prints) nil.
;;## 
;;## ;;;; A hack to return the right value (and unfortunately print twice, once
;;## ;;;; pretty and once not).
;;## 
;;## (cond
;;##  ((equal nrepl-current-version "0.1.7")
;;##   (defun nrepl-send-input (&optional newline)
;;##   "Goto to the end of the input and send the current input.
;;## If NEWLINE is true then add a newline at the end of the input."
;;##   (unless (nrepl-in-input-area-p)
;;##     (error "No input at point"))
;;##   (goto-char (point-max))
;;##   (let ((end (point)))             ; end of input, without the newline
;;##     (nrepl-add-to-input-history (buffer-substring nrepl-input-start-mark end))
;;##     (when newline
;;##       (insert "\n")
;;##       (nrepl-show-maximum-output))
;;##     (let ((inhibit-modification-hooks t))
;;##       (add-text-properties nrepl-input-start-mark
;;##                            (point)
;;##                            `(nrepl-old-input
;;##                              ,(incf nrepl-old-input-counter))))
;;##     (let ((overlay (make-overlay nrepl-input-start-mark end)))
;;##       ;; These properties are on an overlay so that they won't be taken
;;##       ;; by kill/yank.
;;##       (overlay-put overlay 'read-only t)
;;##       (overlay-put overlay 'face 'nrepl-input-face)))
;;##   (let* ((input (nrepl-current-input))
;;##          (form (if (and (not (string-match "\\`[ \t\r\n]*\\'" input)) nrepl-use-pretty-printing)
;;##                    ;; jsk changes here:
;;##                    ;; was: (format "(clojure.pprint/pprint %s)" input)
;;##                    (format "(let [res %s]
;;##                               (clojure.pprint/pprint res)
;;##                               (print \"____\")
;;##                               res)"
;;##                            input)
;;##                  input)))
;;##     (goto-char (point-max))
;;##     (nrepl-mark-input-start)
;;##     (nrepl-mark-output-start)
;;##     (nrepl-send-string form (nrepl-handler (current-buffer)) nrepl-buffer-ns)))))

;;;; ___________________________________________________________________________
;;;; ---- Utility functions ----

(defun nomis-looking-at-whitespace ()
  ;; (looking-at "[:space:]")
  ;; (looking-at "\\s-")
  ;; Neither of the above work, but IIUC they should.
  (looking-at "[ \t\n]"))

(defun nomis-looking-at-sexp-start ()
  (-some-p #'looking-at '("(" "\\[" "{" "#{")))

(defun nomis-looking-at-sexp-end ()
  (and (not (nomis-looking-at-sexp-start))
       (save-excursion
         (backward-char 1)
         (-some-p #'looking-at '(")" "]" "}")))))

(defun nomis-move-to-start-of-sexp-around-point ()
  (cond ((nomis-looking-at-sexp-start)
         ;; stay here
         )
        ((or (nomis-looking-at-whitespace)
             (nomis-looking-at-sexp-end))
         (backward-sexp 1))
        (t
         (ignore-errors (forward-sexp 1))
         (backward-sexp 1))))

(defun nomis-beginning-of-this-defun ()
  ;; this deals with most situations
  (while (ignore-errors (paredit-backward-up) t))
  ;; Check for Clojure #{
  (when (and (looking-at "{")
             (ignore-errors
               (save-excursion
                 (backward-char 1)
                 (looking-at "#"))))
    (backward-char 1))
  ;; this handles the case when we are between top-level forms
  (when (not (nomis-looking-at-sexp-start))
    (backward-sexp)))

(defun* nomis-grab-text (&key top-level-p delete-p)
  (let* ((grab-function (if delete-p
                            #'delete-and-extract-region
                          #'buffer-substring)))
    (save-excursion
      (cond
       (top-level-p
        (let ((start (save-excursion
                       (nomis-beginning-of-this-defun)
                       (point)))
              (end (save-excursion
                     (nomis-beginning-of-this-defun)
                     (forward-sexp 1)
                     (point))))
          (funcall grab-function start end)))
       (t
        (let* ((region-selected?
                (not (equal mark-active nil))))
          (cond
           (region-selected?
            (funcall grab-function (mark) (point)))
           (t
            (nomis-move-to-start-of-sexp-around-point)
            (let ((start (point))
                  (end (save-excursion
                         (forward-sexp 1)
                         (point))))
              (funcall grab-function start end))))))))))

;;## ;;;; ___________________________________________________________________________
;;;; ---- nomis-cider-send-to-repl ----

;;;; Inspired by https://gist.github.com/4349847
;;;; ...which says...
;;;;     inspired by http://bc.tech.coop/blog/070424.html

(define-key clojure-mode-map (kbd "C-H-,")
  'nomis-cider-send-to-repl-selection-or-form-around-point)
(define-key clojure-mode-map (kbd "C-H-.")
  'nomis-cider-send-to-repl-top-level-form)
(define-key clojure-mode-map (kbd "C-H-/")
  'nomis-cider-send-to-repl-after-forward-sexp)
(define-key clojure-mode-map (kbd "C-<kp-enter>")
  'nomis-cider-send-to-repl-return)

(defun nomis-cider-send-to-repl-selection-or-form-around-point (arg)
  "Send text to the REPL.
The text to send:
- If a region is selected, use that text.
- Otherwise use the s-expression around point.
Control of evaluation:
- If no prefix argument is supplied, evaluate the form and do not
  change which window is active.
- If a prefix argument is supplied, do not evaluate the form and
  make the REPL window active."
  (interactive "P")
  (nomis-cider-send-to-repl-helper arg :send-selection-or-form-around-point))

(defun nomis-cider-send-to-repl-top-level-form (arg)
  "Send text to the REPL.
The text to send:
- The top-level s-expression around point.
Control of evaluation:
- If no prefix argument is supplied, evaluate the form and do not
  change which window is active.
- If a prefix argument is supplied, do not evaluate the form and
  make the REPL window active."
  (interactive "P")
  (nomis-cider-send-to-repl-helper arg :send-top-level-form))

(defun nomis-cider-send-to-repl-after-forward-sexp (arg)
  "Send next form to the REPL and move past it (so this
command can be repeated usefully).
Control of evaluation:
- If no prefix argument is supplied, evaluate the form and do not
  change which window is active.
- If a prefix argument is supplied, do not evaluate the form and
  make the REPL window active."
  (interactive "P")
  (forward-sexp)
  (nomis-cider-send-to-repl-helper arg :send-selection-or-form-around-point))

(defun nomis-cider-send-to-repl-return ()
  "Send RETURN to the REPL."
  (interactive)
  (nomis-cider-send-to-repl-helper nil :send-return))

(defcustom nomis-cider-send-to-repl-always-p nil
  "When sending forms to Cider REPL, whether to not check that buffer namespace is same as REPL namespace.")

(defcustom nomis-cider-send-to-buffer-print-newline-first-p nil ; because you always have a newline now -- you changed the prompt to have a newline at the end
  "When sending forms to Cider REPL, whether to send a newline first.")

(defcustom nomis-cider-send-to-buffer-do-return-first-p nil
  "When sending forms to Cider REPL, whether to send a RETURN first (to get a fresh prompt even after output appearing in the REPL buffer).")

(defun nomis-cider-send-to-repl-helper (arg action)
  ;; TODO: Maybe instead of ACTION, should have a function to do whatever.
  (when (or nomis-cider-send-to-repl-always-p
            (null (nomis-clojure-buffer-ns))
            (equal (nomis-clojure-buffer-ns)
                   (nomis-cider-repl-namespace))
            (let ((user-happy-with-namespace-p
                   (y-or-n-p
                    (format "Buffer ns (%s) and REPL ns (%s) are different.
Really send to REPL? "
                            (nomis-clojure-buffer-ns)
                            (nomis-cider-repl-namespace)))))
              (if user-happy-with-namespace-p
                  t
                (error "Not in this namespace!"))))
    (labels ((grab-text
              (top-level-p)
              (nomis-grab-text :top-level-p top-level-p :delete-p nil))
             (the-text
              ()
              (case action
                ((:send-top-level-form) (grab-text t))
                ((:send-selection-or-form-around-point) (grab-text nil))
                ((:send-return) nil)
                (t (error "Bad action")))) 
             (show-cider-repl-buffer-and-send-text-to-it
              (text)
              (labels ((insert-text () (insert text)))
                (let* ((original-frame (selected-frame))
                       (original-window (selected-window)))
                  (set-buffer (nomis-cider-find-or-create-repl-buffer))
                  (unless (eq (current-buffer) (window-buffer))
                    (let* ((window (get-buffer-window (current-buffer)
                                                      t)))
                      (if window
                          (progn
                            (select-frame-set-input-focus (window-frame window))
                            (select-window window))
                        (pop-to-buffer (current-buffer) t))))
                  (goto-char (point-max))
                  (unless (null text)
                    (when nomis-cider-send-to-buffer-print-newline-first-p
                      (newline))
                    (when nomis-cider-send-to-buffer-do-return-first-p
                      (cider-repl-return)
                      (sleep-for 0.25))
                    (insert-text)
                    (backward-sexp)
                    (paredit-reindent-defun)
                    (forward-sexp))
                  (when (null arg)
                    (cider-repl-return)
                    (select-frame-set-input-focus original-frame)
                    (select-window original-window))))))
      (show-cider-repl-buffer-and-send-text-to-it (the-text)))))


;;## ;;;; ___________________________________________________________________________
;;## ;;;; ---- nomis-cider-rearrange-string-into-lines ----
;;## 
;;## ;;;; ****
;;## ;;;; + Ring bell when you get a Clojure error.
;;## ;;;;   Need to write something a bit different to `nrepl-eval-print'.
;;## ;;;;
;;## ;;;; + Either understand Elisp `format' or find a `cl-format' for Emacs.
;;## ;;;;   There is a CL format, but using it changed the current buffer to
;;## ;;;;   *scratch*.  Bad.  Got rid of it.
;;## ;;;;
;;## ;;;; + Ensure `nomis-grab-text' has no free variables.
;;## ;;;;
;;## ;;;; + Put all your code-manipulation Clojure functions in single file in
;;## ;;;;   a new project.
;;## ;;;;   And have proper tests of the code-manipulation code.
;;## ;;;;
;;## ;;;; - When to load this file the Clojure file?
;;## ;;;;   - The Right Thing
;;## ;;;;     - How do you set up dev dependencies?
;;## ;;;;       (If you could do this, you could have Leiningen load up the Clojure
;;## ;;;;       code.)
;;## ;;;;       A :user profile in "~/.lein/profiles.clj'.
;;## ;;;;       Do I need to set up a local repository? (Or put things somewhere
;;## ;;;;       remote?)
;;## ;;;;   x Ignore the following.  Do The Right Thing.
;;## ;;;;     x For now you are sending the Clojure code to the Clojure world every
;;## ;;;;       time.
;;## ;;;;     x I had wanted to load it in some after advice to `nrepl-jack-in',
;;## ;;;;       but you'd have to wait somehow for the server to finish starting.

(defvar nomis-newline-string "
")

(defun transform-string-value (value)
  (replace-regexp-in-string
   "\\\\n" nomis-newline-string ; replace all \n with newline
   value))

;;## (defun get-string-from-file (filePath)
;;##   "Return FILEPATH's file content."
;;##   ;; http://xahlee.blogspot.co.uk/2010/09/elisp-read-file-content-in-one-shot.html
;;##   ;; which says:
;;##   ;;   thanks to “Pascal J Bourguignon”
;;##   ;;   and "TheFlyingDutchman <zzbba...@aol.com>". 2010-09-02
;;##   ;; 
;;##   ;; I changed insert-file-contents to insert-file-contents-literally
;;##   (with-temp-buffer
;;##     (insert-file-contents-literally filePath)
;;##     (buffer-string)))

(defun nomis-cider-rearrange-string-into-lines (prefix)
  "Rearrange string into lines.
   Without a prefix argument, indent second and subsequent lines so
   that they line up sensibly with the first line.
   With a prefix argument, indent second and subsequent lines one
   character less as is the convention for Clojure doc strings
   (which is stupid)."
  (interactive "*P")
  (let* ((string (nomis-grab-text
                  :top-level-p nil
                  :delete-p t))
         (clojure-form-as-string
          (format "(do (require '[com.nomistech.emacs-hacks-in-clojure :as ehic])
                  (ehic/rearrange-string-into-lines '%s %s %s))"
                  string
                  (+ (current-column)
                     (if prefix 0 1))
                  72))
         (string-value (cider-eval-and-get-value clojure-form-as-string
                                                 nrepl-buffer-ns)))
    (save-excursion
      (insert
       (format "\"%s\""
               (transform-string-value string-value))))))

(define-key clojure-mode-map (kbd "C-c C-g")
  'nomis-cider-rearrange-string-into-lines)

;;;; ___________________________________________________________________________
;;;; ---- Reader comments ----

;;;; Inspired by https://gist.github.com/4349847
;;;; ...which says...
;;;;     inspired by http://bc.tech.coop/blog/070122.html
;;;;     ported from slime/contrib/slime-editing-commands.el

(define-key clojure-mode-map (kbd "C-c ;")
  'nomis-cider-insert-reader-comment)
(define-key clojure-mode-map (kbd "C-c M-;")
  'nomis-cider-remove-reader-comment)

(defun nomis-cider-insert-reader-comment (prefix)
  "Insert a reader comment (#_) around the s-expression containing the point.
If this command is invoked repeatedly (without any other command
occurring between invocations), the comment progressively moves outward
over enclosing expressions. If invoked with a positive prefix argument,
the s-expression prefix expressions out is enclosed in a set of balanced
comments."
  (interactive "*p")
  (save-excursion
    (if (eq last-command this-command)
        (when (search-backward "#_" nil t) ; wrong -- not structure-aware
          (delete-char 2)
          (backward-up-list)
          (insert "#_"))
      (progn
        (nomis-move-to-start-of-sexp-around-point)
        (dotimes (i (1- prefix))
          (backward-up-list)
          (decf prefix))
        (insert "#_")))))

(defun nomis-cider-remove-reader-comment ()
  "Remove a reader comment enclosing point."
  (interactive "*")
  (save-excursion
    (when (search-backward "#_" nil t) ; wrong -- not structure-aware
      (delete-char 2))))

;;## ;;;; ___________________________________________________________________________
;;## 
;;## ;;;; TODO: Tidy; just hacked for now.
;;## 
;;## ;; Alternative approach (From
;;## ;; https://news.ycombinator.com/item?id=5819487) This technique will
;;## ;; display the output in the minibuffer, the latter will display it in
;;## ;; the repl.
;;## 
;;## (defun nrepl-refresh ()
;;##   (interactive)
;;##   (nrepl-interactive-eval "(clojure.tools.namespace.repl/refresh)"))
;;## 
;;## (defun nrepl-reset ()
;;##   (interactive)
;;##   (nrepl-interactive-eval "(user/reset)"))
;;## 
;;## (defun nrepl-refresh-to-repl ()
;;##   (interactive)
;;##   (set-buffer "*nrepl*")
;;##   (goto-char (point-max))
;;##   (insert "(clojure.tools.namespace.repl/refresh)")
;;##   (nrepl-return)
;;##   ;; (goto-char (point-max))
;;##   )
;;## 
;;## (defun nrepl-reset-to-repl ()
;;##   (interactive)
;;##   (set-buffer "*nrepl*")
;;##   (goto-char (point-max))
;;##   (insert "(user/reset)")
;;##   (nrepl-return)
;;##   ;; (goto-char (point-max))
;;##   )

;;;; ___________________________________________________________________________
;;;; ---- Set up `cider-repl-history-file` on a per-project basis ----

;;;; THIS WILL NOT WORK WELL IF YOU HAVE MULTIPLE REPL SESSIONS
;;;; IN A SINGLE EMACS INSTANCE.

(cond
 ((member (cider-version)
          '("CIDER 0.6.0"))
  (defun cider-jack-in (&optional prompt-project)
    "Start a nREPL server for the current project and connect to it.
If PROMPT-PROJECT is t, then prompt for the project for which to
start the server."
    (interactive "P")
    (setq cider-current-clojure-buffer (current-buffer))
    (let* ((project (when prompt-project
                      (read-directory-name "Project: ")))
           (project-dir (nrepl-project-directory-for
                         (or project (nrepl-current-dir)))))
      (progn ; jsk: Added this stuff
        (let ((nomis-cider-repl-history-file
               (concat project-dir ".cider-repl-history")))
          ;; (message-box (concat "Setting cider-repl-history-file to "
          ;;                      nomis-cider-repl-history-file))
          (setq cider-repl-history-file nomis-cider-repl-history-file)))
      (when (nrepl-check-for-repl-buffer nil project-dir)
        (let* ((nrepl-project-dir project-dir)
               (cmd (if project
                        (format "cd %s && %s" project cider-server-command)
                      cider-server-command))
               (default-directory (or project-dir default-directory))
               (nrepl-buffer-name (generate-new-buffer-name
                                   (nrepl-server-buffer-name)))
               (process
                (progn
                  ;; the buffer has to be created before the proc:
                  (get-buffer-create nrepl-buffer-name)
                  (start-file-process-shell-command
                   "nrepl-server"
                   nrepl-buffer-name
                   cmd))))
          (set-process-filter process 'nrepl-server-filter)
          (set-process-sentinel process 'nrepl-server-sentinel)
          (set-process-coding-system process 'utf-8-unix 'utf-8-unix)
          (with-current-buffer (process-buffer process)
            (setq nrepl-project-dir project-dir))
          (message "Starting nREPL server..."))))))
 ((member (cider-version)
          '("CIDER 0.7.0"))
  (defun cider-jack-in (&optional prompt-project)
    "Start a nREPL server for the current project and connect to it.
If PROMPT-PROJECT is t, then prompt for the project for which to
start the server."
    (interactive "P")
    (setq cider-current-clojure-buffer (current-buffer))
    (if (cider--lein-present-p)
        (let* ((project (when prompt-project
                          (read-directory-name "Project: ")))
               (project-dir (nrepl-project-directory-for
                             (or project (nrepl-current-dir))))
               (server-command (if prompt-project
                                   (read-string (format "Server command: %s " cider-lein-command) cider-lein-parameters)
                                 cider-lein-parameters)))
          (progn ; jsk: Added this stuff
            (let ((nomis-cider-repl-history-file
                   (concat project-dir ".cider-repl-history")))
              ;; (message-box (concat "Setting cider-repl-history-file to "
              ;;                      nomis-cider-repl-history-file))
              (setq cider-repl-history-file nomis-cider-repl-history-file)))
          (when (nrepl-check-for-repl-buffer nil project-dir)
            (let* ((nrepl-project-dir project-dir)
                   (cmd (format "%s %s" cider-lein-command cider-lein-parameters))
                   (default-directory (or project-dir default-directory))
                   (nrepl-buffer-name (generate-new-buffer-name
                                       (nrepl-server-buffer-name)))
                   (process
                    (progn
                      ;; the buffer has to be created before the proc:
                      (get-buffer-create nrepl-buffer-name)
                      (start-file-process-shell-command
                       "nrepl-server"
                       nrepl-buffer-name
                       cmd))))
              (set-process-filter process 'nrepl-server-filter)
              (set-process-sentinel process 'nrepl-server-sentinel)
              (set-process-coding-system process 'utf-8-unix 'utf-8-unix)
              (with-current-buffer (process-buffer process)
                (setq nrepl-project-dir project-dir))
              (message "Starting nREPL server via %s..."
                       (propertize cmd 'face 'font-lock-keyword-face)))))
      (message "The %s executable (specified by `cider-lein-command') isn't on your exec-path" cider-lein-command))))
 ((member (cider-version)
          '("CIDER 0.8.2"))
  (defun cider-jack-in (&optional prompt-project)
    "Start a nREPL server for the current project and connect to it.
If PROMPT-PROJECT is t, then prompt for the project for which to
start the server."
    (interactive "P")
    (setq cider-current-clojure-buffer (current-buffer))
    (if (cider--lein-present-p)
        (let* ((project (when prompt-project
                          (read-directory-name "Project: ")))
               (project-dir (nrepl-project-directory-for
                             (or project (nrepl-current-dir))))
               (lein-params (if prompt-project
                                (read-string (format "nREPL server command: %s "
                                                     cider-lein-command)
                                             cider-lein-parameters)
                              cider-lein-parameters))
               (cmd (format "%s %s" cider-lein-command lein-params)))
          (progn ; jsk: Added this stuff
            (let ((nomis-cider-repl-history-file
                   (concat project-dir ".cider-repl-history")))
              ;; (message-box (concat "Setting cider-repl-history-file to "
              ;;                      nomis-cider-repl-history-file))
              (setq cider-repl-history-file nomis-cider-repl-history-file)))
          (when (nrepl-check-for-repl-buffer nil project-dir)
            (nrepl-start-server-process project-dir cmd)))
      (message "The %s executable (specified by `cider-lein-command') isn't on your exec-path"
               cider-lein-command))))
 (t
  (message-box
   "You need to fix your Cider REPL history file stuff for this version of Cider.")))

;;;; ___________________________________________________________________________
;;;; ---- Allow Emacs to quit when a project has been moved ----

(cond
 ((member (cider-version)
          '("CIDER 0.7.0"
            "CIDER 0.8.2"))
  (defun cider-repl--history-write (filename)
    "Write history to FILENAME.
Currently coding system for writing the contents is hardwired to
utf-8-unix."
    (let* ((mhist (cider-repl--histories-merge cider-repl-input-history
                                               cider-repl-input-history-items-added
                                               (cider-repl--history-read filename)))
           ;; newest items are at the beginning of the list, thus 0
           (hist (cl-subseq mhist 0 (min (length mhist) cider-repl-history-size))))
      (if (not (file-writable-p filename))
          (message-box "History file not writable: %s" filename)
        (let ((print-length nil) (print-level nil))
          (with-temp-file filename
            ;; TODO: really set cs for output
            ;; TODO: does cs need to be customizable?
            (insert ";; -*- coding: utf-8-unix -*-\n")
            (insert ";; Automatically written history of CIDER REPL session\n")
            (insert ";; Edit at your own risk\n\n")
            (prin1 (mapcar #'substring-no-properties hist) (current-buffer))))))))
 (t
  (message-box
   "You need to fix your cider-repl--history-write stuff for this version of Cider.")))

;;;; ___________________________________________________________________________

(provide 'nomis-cider-extras)
