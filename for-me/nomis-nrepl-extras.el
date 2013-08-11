;;;; Init stuff -- nrepl extras

;;;; ___________________________________________________________________________
;;;; ---- Wrappers for things in nrepl.el, to isolate dependencies and make ----
;;;; ---- it easier to upgrade nrepl.el.                                    ----

(defun nomis-nrepl-find-or-create-repl-buffer ()
  (cond ((member nrepl-current-version '("0.1.7"))
         (nrepl-find-or-create-repl-buffer))
        (t
         (error "nomis-nrepl-find-or-create-repl-buffer: need to update for nrepl.el %s"
                nrepl-current-version))))


;;;; ___________________________________________________________________________
;;;; ---- Cause focus to go to stacktrace window when popped up -----

(cond

 ((equal nrepl-current-version "0.1.6")
  (defadvice nrepl-default-err-handler (after select-nrepl-error-buffer ())
    (when (or nrepl-popup-stacktraces
              (not (member (buffer-local-value 'major-mode buffer)
                           '(nrepl-mode clojure-mode))))
      (select-window (get-buffer-window nrepl-error-buffer))))
  (ad-activate 'nrepl-default-err-handler))

 ((equal nrepl-current-version "0.1.7")
  ;; Too hard to work out suitable advice, so redefine the function
  ;; with a change.
  (defun nrepl-default-err-handler (buffer ex root-ex session)
    "Make an error handler for BUFFER, EX, ROOT-EX and SESSION."
    ;; TODO: use ex and root-ex as fallback values to display when pst/print-stack-trace-not-found
    (let ((replp (equal 'nrepl-mode (buffer-local-value 'major-mode buffer))))
      (if (or (and nrepl-popup-stacktraces-in-repl replp)
              (and nrepl-popup-stacktraces (not replp)))
          (lexical-let ((nrepl-popup-on-error nrepl-popup-on-error))
            (with-current-buffer buffer
              (nrepl-send-string "(if-let [pst+ (clojure.core/resolve 'clj-stacktrace.repl/pst+)]
                        (pst+ *e) (clojure.stacktrace/print-stack-trace *e))"
                                 (nrepl-make-response-handler
                                  (nrepl-make-popup-buffer nrepl-error-buffer)
                                  nil
                                  (lambda (buffer value)
                                    (nrepl-emit-into-color-buffer buffer value)
                                    (when nrepl-popup-on-error
                                      (nrepl-popup-buffer-display buffer
                                                                  t ; jsk change
                                                                  )))
                                  nil nil) nil session))
            (with-current-buffer nrepl-error-buffer
              (compilation-minor-mode +1))))))))

;;;; ___________________________________________________________________________
;;;; ---- Cause evaluation to happen in correct namespace ----

;;;; **** Are there any more functions to advise to get evaluation in right ns?

;;;; Either this, or remember to do `nrepl-eval-ns-form' in each buffer (or
;;;; something), which gets over the problem.

(defmacro nomis-nrepl-with-right-namespace (&rest body)
  `(let ((nrepl-buffer-ns (or *my-hacky-ns* (nrepl-find-ns))))
     ,@body))

(when (member nrepl-current-version '("0.1.6"
                                      "0.1.7"))

  (defvar *my-hacky-ns* nil)

  (progn
    (defadvice nrepl-interactive-eval (around eval-in-correct-namespace)
      (nomis-nrepl-with-right-namespace
       ad-do-it))
    (ad-activate 'nrepl-interactive-eval))

  (progn
    (defadvice nrepl-interactive-eval-print (around eval-in-correct-namespace)
      (nomis-nrepl-with-right-namespace
       ad-do-it))
    (ad-activate 'nrepl-interactive-eval-print))

  (progn
    (defadvice nrepl-eval-ns-form (around eval-in-correct-namespace)
      (let ((*my-hacky-ns* "user"))
        ad-do-it))
    (ad-activate 'nrepl-eval-ns-form)))


;;;; ___________________________________________________________________________
;;;; ---- Prompt ----

;;;; I want to enter my input on a fresh line. Nice when you are in a
;;;; namespace that has a long name.

(cond
 ((equal nrepl-current-version "0.1.7")
  (defun nrepl-insert-prompt (namespace)
    "Insert the prompt (before markers!), taking into account NAMESPACE.
Set point after the prompt.
Return the position of the prompt beginning."
    (goto-char nrepl-input-start-mark)
    (nrepl-save-marker nrepl-output-start
      (nrepl-save-marker nrepl-output-end
        (unless (bolp) (insert-before-markers "\n"))
        (let ((prompt-start (point))
              (prompt (let ((original-prompt (format "%s>" namespace)))
                        ;; jsk: Added stuff here
                        (concatenate 'string
                                     ;; (make-string 80 ?\_)
                                     "\n"
                                     original-prompt
                                     "\n"))))
          (nrepl-propertize-region
              '(face nrepl-prompt-face read-only t intangible t
                     nrepl-prompt t
                     rear-nonsticky (nrepl-prompt read-only face intangible))
            (insert-before-markers prompt))
          (set-marker nrepl-prompt-start-mark prompt-start)
          prompt-start))))))

;;;; ___________________________________________________________________________
;;;; ---- Pretty printing of results ----

;;;; Pretty printing of results is broken, because:
;;;; - It returns (and prints) nil.

;;;; A hack to return the right value (and unfortunately print twice, once
;;;; pretty and once not).

(cond
 ((equal nrepl-current-version "0.1.7")
  (defun nrepl-send-input (&optional newline)
  "Goto to the end of the input and send the current input.
If NEWLINE is true then add a newline at the end of the input."
  (unless (nrepl-in-input-area-p)
    (error "No input at point"))
  (goto-char (point-max))
  (let ((end (point)))             ; end of input, without the newline
    (nrepl-add-to-input-history (buffer-substring nrepl-input-start-mark end))
    (when newline
      (insert "\n")
      (nrepl-show-maximum-output))
    (let ((inhibit-modification-hooks t))
      (add-text-properties nrepl-input-start-mark
                           (point)
                           `(nrepl-old-input
                             ,(incf nrepl-old-input-counter))))
    (let ((overlay (make-overlay nrepl-input-start-mark end)))
      ;; These properties are on an overlay so that they won't be taken
      ;; by kill/yank.
      (overlay-put overlay 'read-only t)
      (overlay-put overlay 'face 'nrepl-input-face)))
  (let* ((input (nrepl-current-input))
         (form (if (and (not (string-match "\\`[ \t\r\n]*\\'" input)) nrepl-use-pretty-printing)
                   ;; jsk changes here:
                   ;; was: (format "(clojure.pprint/pprint %s)" input)
                   (format "(let [res %s]
                              (clojure.pprint/pprint res)
                              (print \"____\")
                              res)"
                           input)
                 input)))
    (goto-char (point-max))
    (nrepl-mark-input-start)
    (nrepl-mark-output-start)
    (nrepl-send-string form (nrepl-handler (current-buffer)) nrepl-buffer-ns)))))

;;;; ___________________________________________________________________________
;;;; ---- Namespace functions ----

(defun nomis-nrepl-repl-namespace ()
  (with-current-buffer "*nrepl*"
    nrepl-buffer-ns))

(defun nomis-nrepl-buffer-namespace-is-repl-namespace-p ()
  (equal (nrepl-find-ns)
         (nomis-nrepl-repl-namespace)))

;;;; ___________________________________________________________________________
;;;; ---- Utility functions ----

(defun nomis-looking-at-whitespace ()
  ;; (looking-at "[:space:]")
  ;; (looking-at "\\s-")
  ;; Neither of the above work, but IIUC they should.
  (looking-at "[ \t\n]"))

(defun nomis-looking-at-sexp-start ()
  (looking-at "(")
  )

(defun nomis-looking-at-sexp-end ()
  (and (not (nomis-looking-at-sexp-start))
       (save-excursion
         (backward-char 1)
         (looking-at ")"))))

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
  ;; this handles the case when we are between top-level forms
  (when (not (nomis-looking-at-sexp-start))
    (backward-sexp)))

(defun* nomis-nrepl-grab-text (&key top-level-p delete-p)
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

;;;; ___________________________________________________________________________
;;;; ---- nomis-nrepl-send-to-repl ----

;;;; Inspired by https://gist.github.com/4349847
;;;; ...which says...
;;;;     inspired by http://bc.tech.coop/blog/070424.html

(define-key nrepl-interaction-mode-map (kbd "C-x C-/")
  'nomis-nrepl-send-to-repl-selection-or-form-around-point)
(define-key nrepl-interaction-mode-map (kbd "C-M-g")
  'nomis-nrepl-send-to-repl-top-level-form)

(defun nomis-nrepl-send-to-repl-selection-or-form-around-point (arg)
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
  (nomis-nrepl-send-to-repl-helper arg nil))

(defun nomis-nrepl-send-to-repl-top-level-form (arg)
  "Send text to the REPL.
The text to send:
- The top-level s-expression around point.
Control of evaluation:
- If no prefix argument is supplied, evaluate the form and do not
  change which window is active.
- If a prefix argument is supplied, do not evaluate the form and
  make the REPL window active."
  (interactive "P")
  (nomis-nrepl-send-to-repl-helper arg t))

(defvar *nrepl-send-to-buffer-print-newline-first* nil) ; because you always have a newline now -- you changed the prompt to have a newline at the end

(defun nomis-nrepl-send-to-repl-helper (arg top-level-p)
  (when (or (nomis-nrepl-buffer-namespace-is-repl-namespace-p)
            (y-or-n-p
             (format "Buffer ns (%s) and REPL ns (%s) are different.
Really send to REPL? "
                     (nrepl-find-ns)
                     (nomis-nrepl-repl-namespace))))
    (labels ((the-text
              ()
              (nomis-nrepl-grab-text :top-level-p top-level-p :delete-p nil))
             (show-nrepl-buffer-and-send-text-to-it
              (text)
              (labels ((insert-text () (insert text)))
                (let* ((original-window (selected-window)))
                  (set-buffer (nomis-nrepl-find-or-create-repl-buffer))
                  (unless (eq (current-buffer) (window-buffer))
                    (pop-to-buffer (current-buffer) t))
                  (goto-char (point-max))
                  (when *nrepl-send-to-buffer-print-newline-first*
                    (newline))
                  (insert-text)
                  (backward-sexp)
                  (indent-pp-sexp) ; (paredit-reindent-defun) ; nrepl doesn't indent (yet)
                  (forward-sexp)
                  (when (null arg)
                    (nrepl-return)
                    (select-window original-window))))))
      (show-nrepl-buffer-and-send-text-to-it (the-text)))))


;;;; ___________________________________________________________________________
;;;; ---- nomis-nrepl-rearrange-string-into-lines ----

;;;; ****
;;;; + Ring bell when you get a Clojure error.
;;;;   Need to write something a bit different to `nrepl-eval-print'.
;;;;
;;;; + You have two-step undo. Can you have one-step undo?
;;;; 
;;;; + Either understand Elisp `format' or find a `cl-format' for Emacs.
;;;;   There is a CL format, but using it changed the current buffer to
;;;;   *scratch*.  Bad.  Got rid of it.
;;;;
;;;; + Ensure `nomis-nrepl-grab-text' has no free variables.
;;;;
;;;; + Modularise `nomis-nrepl-grab-text' and `nomis-nrepl-grab-and-delete-current-form'.
;;;;
;;;; + Put all your code-manipulation Clojure functions in single file in
;;;;   a new project.
;;;;   And have proper tests of the code-manipulation code.
;;;;
;;;; - When to load this file the Clojure file?
;;;;   - The Right Thing
;;;;     - How do you set up dev dependencies?
;;;;       (If you could do this, you could have Leiningen load up the Clojure
;;;;       code.)
;;;;       A :user profile in "~/.lein/profiles.clj'.
;;;;       Do I need to set up a local repository? (Or put things somewhere
;;;;       remote?)
;;;;   x Ignore the following.  Do The Right Thing.
;;;;     x For now you are sending the Clojure code to the Clojure world every
;;;;       time.
;;;;     x I had wanted to load it in some after advice to `nrepl-jack-in',
;;;;       but you'd have to wait somehow for the server to finish starting.
;;;;
;;;; + Change all `jsk' to `nomis.'
;;;;
;;;; + Choose between `nomis-' prefix and `nomis-' prefix.
;;;;
;;;; + Compare nrepl-last-expression with how you get an expression.
;;;;   + Mine is better.

(defparameter nomis-rearrange-string-in-one-go-p t
  "Having this T means that undoing a nomis-nrepl-rearrange-string-into-lines
undoes the whole thing.
Having this NIL gives a two-step undo.
Before this was introduced, I had the two-step behaviour.
I'm more confident that things works properly that way.
When T I'm unsure about maybe destroying info when grabbing text, particularly
the mark-active thing. It all seems to be ok though.")

(defparameter nomis-newline-string "
")

(defun transform-string-value (value)
  (replace-regexp-in-string
   "\\\\n" nomis-newline-string         ; replace all \n with newline
   value))

(defun* nomis-nrepl-interactive-eval-print-handler-with-bells-on
    (buffer
     &key
     save-excursion-p)
  ;; Based on `nrepl-interactive-eval-print-handler'.
  (lexical-let* ((save-excursion-p save-excursion-p))
    (nrepl-make-response-handler
     buffer
     (lambda (buffer value)
       (with-current-buffer buffer
         (flet ((do-it
                 ()
                 (when nomis-rearrange-string-in-one-go-p
                   (nomis-nrepl-grab-text :top-level-p nil
                                          :delete-p t))
                 (insert
                  (format "%s"
                          (transform-string-value value)))))
           (if save-excursion-p
               (save-excursion (do-it))
             (do-it)))))
     '()
     (lambda (buffer err)
       (ding)
       (message "%s" err))
     '())))

(defun* nomis-nrepl-interactive-eval-print-with-bells-on
    (form &key save-excursion-p)
  "Evaluate the given form and print value in current buffer.
Ring the bell if there's an error in the Clojure world."
  ;; Based on `nrepl-interactive-eval-print'.
  (let ((buffer (current-buffer)))
    (nrepl-send-string form
                       (nomis-nrepl-interactive-eval-print-handler-with-bells-on
                        buffer
                        :save-excursion-p save-excursion-p)
                       nrepl-buffer-ns)))

(defun get-string-from-file (filePath)
  "Return FILEPATH's file content."
  ;; http://xahlee.blogspot.co.uk/2010/09/elisp-read-file-content-in-one-shot.html
  ;; which says:
  ;;   thanks to “Pascal J Bourguignon”
  ;;   and "TheFlyingDutchman <zzbba...@aol.com>". 2010-09-02
  ;; 
  ;; I changed insert-file-contents to insert-file-contents-literally
  (with-temp-buffer
    (insert-file-contents-literally filePath)
    (buffer-string)))

(defun nomis-nrepl-rearrange-string-into-lines (prefix)
  "Rearrange string into lines.
   Without a prefix argument, indent second and subsequent lines so
   that they line up sensibly with the first line.
   With a prefix argument, indent second and subsequent lines one
   character less as is the convention for Clojure doc strings
   (which is stupid)."
  (interactive "*P")
  (let ((string (nomis-nrepl-grab-text
                 :top-level-p nil
                 :delete-p (not nomis-rearrange-string-in-one-go-p))))
    (nomis-nrepl-interactive-eval-print-with-bells-on
     (format "(do (require '[com.nomistech.emacs-hacks-in-clojure :as ehic])
                  (ehic/rearrange-string-into-lines '%s %s %s))"
             string
             (+ (current-column)
                (if prefix 0 1))
             72)
     :save-excursion-p t)))

(define-key clojure-mode-map (kbd "C-c C-f")
  'nomis-nrepl-rearrange-string-into-lines)


;;;; ___________________________________________________________________________
;;;; ---- Reader comments ----

;;;; Inspired by https://gist.github.com/4349847
;;;; ...which says...
;;;;     inspired by http://bc.tech.coop/blog/070122.html
;;;;     ported from slime/contrib/slime-editing-commands.el

(define-key clojure-mode-map (kbd "C-c ;")
  'nomis-nrepl-insert-reader-comment)
(define-key clojure-mode-map (kbd "C-c M-;")
  'nomis-nrepl-remove-reader-comment)

(defun nomis-nrepl-insert-reader-comment (prefix)
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

(defun nomis-nrepl-remove-reader-comment ()
  "Remove a reader comment enclosing point."
  (interactive "*")
  (save-excursion
    (when (search-backward "#_" nil t) ; wrong -- not structure-aware
      (delete-char 2))))

;;;; ___________________________________________________________________________

;;;; TODO: Tidy; just hacked for now.

;; Alternative approach (From
;; https://news.ycombinator.com/item?id=5819487) This technique will
;; display the output in the minibuffer, the latter will display it in
;; the repl.

(defun nrepl-refresh ()
  (interactive)
  (nrepl-interactive-eval "(clojure.tools.namespace.repl/refresh)"))

(defun nrepl-reset ()
  (interactive)
  (nrepl-interactive-eval "(user/reset)"))

(defun nrepl-refresh-to-repl ()
  (interactive)
  (set-buffer "*nrepl*")
  (goto-char (point-max))
  (insert "(clojure.tools.namespace.repl/refresh)")
  (nrepl-return)
  ;; (goto-char (point-max))
  )

(defun nrepl-reset-to-repl ()
  (interactive)
  (set-buffer "*nrepl*")
  (goto-char (point-max))
  (insert "(user/reset)")
  (nrepl-return)
  ;; (goto-char (point-max))
  )

;;;; ___________________________________________________________________________

(provide 'nomis-nrepl-extras)
