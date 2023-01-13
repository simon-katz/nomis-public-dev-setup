;;;; Init stuff -- CIDER extras (old, non-lexical)

;;;; Move this stuff to `nomis-cider-extras`, making any necessary changes to
;;;; make things work with lexical binding.

;;## ;;;; TODO: Look at all this in the light of:
;;## ;;;;       - Now have nrepl.el 0.1.8. (Hmmm, no, reverted to 0.1.7 because eldoc
;;## ;;;;         doesn't hapen with 0.1.8.)
;;## ;;;;         See https://groups.google.com/forum/#!topic/nrepl-el/RZTitQyb6mo.
;;## ;;;;       - Did/does it all make sense anyway?

(require 'cider)
(require 'nomis-run-clojure)

;;;; ___________________________________________________________________________
;;;; ---- nomis/cider-version ----

(defun nomis/cider-version ()
  (let ((inhibit-message t))
    (cider-version)))

(cl-assert (equal (nomis/cider-version)
               (cider-version)))

;;;; ___________________________________________________________________________
;;;; ---- Wrappers for things in Cider, to isolate dependencies and make ----
;;;; ---- it easier to upgrade Cider.                                    ----

(cond
 ((member (nomis/cider-version)
          '("CIDER 0.7.0"))
  (defun nomis/clojure-buffer-ns ()
    (cider-find-ns)))
 (t
  (defun nomis/clojure-buffer-ns ()
    (clojure-find-ns))))

(cond
 ((member (nomis/cider-version)
          '("CIDER 0.7.0"
            "CIDER 0.8.2"))
  (defun nomis/cider-repl-namespace ()
    (with-current-buffer (cider-current-repl-buffer)
      nrepl-buffer-ns)))
 ((member (nomis/cider-version)
          '("CIDER 0.9.1"
            "CIDER 0.10.0"
            "CIDER 0.12.0 (Seattle)"
            "CIDER 0.14.0 (Berlin)"
            "CIDER 0.15.0 (London)"
            "CIDER 0.16.0 (Riga)"))
  (defun nomis/cider-repl-namespace ()
    (with-current-buffer (cider-current-repl-buffer)
      cider-buffer-ns)))
 (t
  (defun nomis/cider-repl-namespace ()
    (with-current-buffer (cider-current-connection)
      cider-buffer-ns))))

(cond
 ((member (nomis/cider-version)
          '("CIDER 0.7.0"))
  (defun nomis/cider-find-or-create-repl-buffer ()
    (cider-find-or-create-repl-buffer)))
 ((member (nomis/cider-version)
          '("CIDER 0.8.2"
            "CIDER 0.9.1"))
  (defun nomis/cider-find-or-create-repl-buffer ()
    (cider-get-repl-buffer)))
 (t
  (defun nomis/cider-find-or-create-repl-buffer ()
    (cider-current-connection))))


;;;; ___________________________________________________________________________
;;;; ---- Prompt ----

;;;; I want to enter my input on a fresh line. Nice when you are in a
;;;; namespace that has a long name.

(defvar nomis/cider-repl--hack-prompt-p t)

(defvar nomis/cider-repl--prompt-prefix (concat (make-string 80 ?\_) "\n"))

(defvar nomis/cider-repl--prompt-suffix "\n")

(cond
 ((member (nomis/cider-version)
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
                        (concat (make-string 80 ?\_)
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
 ((and (member (nomis/cider-version)
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
                        (concat (make-string 80 ?\_)
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
          (cl-labels ((do-it ()
                             (funcall
                              (cond
                               ((member (nomis/cider-version)
                                        '("CIDER 0.10.0"))
                                'cider-repl-default-prompt)
                               (t
                                'cider-repl-prompt-default))
                              namespace)))
            (if nomis/cider-repl--hack-prompt-p
                (concat nomis/cider-repl--prompt-prefix
                        (do-it)
                        nomis/cider-repl--prompt-suffix)
              (do-it))))))
 (t
  (message-box
   "You need to fix your Cider prompt stuff for this version of Cider.")))

;;;; ___________________________________________________________________________
;;;; ---- Utility functions ----

(require 'nomis-sexp-utils)

(cl-defun nomis/grab-text (&key top-level-p delete-p)
  (let* ((grab-function (if delete-p
                            #'delete-and-extract-region
                          #'buffer-substring)))
    (save-excursion
      (cond
       (top-level-p
        (let ((start (save-excursion
                       (nomis/beginning-of-top-level-form)
                       (point)))
              (end (save-excursion
                     (nomis/beginning-of-top-level-form)
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
            (nomis/move-to-start-of-bracketed-sexp-around-point)
            (let ((start (point))
                  (end (save-excursion
                         (forward-sexp 1)
                         (point))))
              (funcall grab-function start end))))))))))

;;## ;;;; ___________________________________________________________________________
;;;; ---- nomis/cider-send-to-repl ----

;;;; Inspired by https://gist.github.com/4349847
;;;; ...which says...
;;;;     inspired by http://bc.tech.coop/blog/070424.html

(define-key cider-mode-map (kbd "H-z H-,")
  'nomis/cider-send-to-repl-selection-or-form-around-point)
(define-key cider-mode-map (kbd "H-z H-.")
  'nomis/cider-send-to-repl-top-level-form)
(define-key cider-mode-map (kbd "H-z H-/")
  'nomis/cider-send-to-repl-after-forward-sexp)
(define-key cider-mode-map (kbd "H-z <kp-enter>")
  'nomis/cider-send-to-repl-return)

(defun nomis/cider-send-to-repl-selection-or-form-around-point (arg)
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
  (nomis/cider-send-to-repl-helper arg :send-selection-or-form-around-point))

(defun nomis/cider-send-to-repl-top-level-form (arg)
  "Send text to the REPL.
The text to send:
- The top-level s-expression around point.
Control of evaluation:
- If no prefix argument is supplied, evaluate the form and do not
  change which window is active.
- If a prefix argument is supplied, do not evaluate the form and
  make the REPL window active."
  (interactive "P")
  (nomis/cider-send-to-repl-helper arg :send-top-level-form))

(defun nomis/cider-send-to-repl-after-forward-sexp (arg)
  "Send next form to the REPL and move past it (so this
command can be repeated usefully).
Control of evaluation:
- If no prefix argument is supplied, evaluate the form and do not
  change which window is active.
- If a prefix argument is supplied, do not evaluate the form and
  make the REPL window active."
  (interactive "P")
  (forward-sexp)
  (nomis/cider-send-to-repl-helper arg :send-selection-or-form-around-point))

(defun nomis/cider-send-to-repl-return ()
  "Send RETURN to the REPL."
  (interactive)
  (nomis/cider-send-to-repl-helper nil :send-return))

(defcustom nomis/cider-send-to-repl-always-p nil
  "When sending forms to Cider REPL, whether to not check that buffer namespace is same as REPL namespace.")

(defcustom nomis/cider-send-to-buffer-print-newline-first-p nil ; because you always have a newline now -- you changed the prompt to have a newline at the end
  "When sending forms to Cider REPL, whether to send a newline first.")

(defcustom nomis/cider-send-to-buffer-do-return-first-p nil
  "When sending forms to Cider REPL, whether to send a RETURN first (to get a fresh prompt even after output appearing in the REPL buffer).")

(defun nomis/cider-send-to-repl-helper (arg action)
  ;; TODO: Maybe instead of ACTION, should have a function to do whatever.
  (cl-labels ((grab-text
               (top-level-p)
               (nomis/grab-text :top-level-p top-level-p :delete-p nil))
              (the-text
               ()
               (cl-case action
                 ((:send-top-level-form)
                  (cl-case :new-experimental
                    (:old
                     (grab-text t))
                    (:new-experimental
                     ;; Use the CIDER experimental functionality that takes into
                     ;; account the value of
                     ;; `clojure-toplevel-inside-comment-form`.
                     (cl-destructuring-bind (start end)
                         (cider-defun-at-point 'bounds)
                       (buffer-substring-no-properties start end)))))
                 ((:send-selection-or-form-around-point) (grab-text nil))
                 ((:send-return) nil)
                 (t (error "Bad action"))))
              (show-cider-repl-buffer-and-send-text-to-it
               (text)
               (cl-labels ((insert-text () (insert text)))
                 (let* ((original-frame (selected-frame))
                        (original-window (selected-window)))
                   (set-buffer (nomis/cider-find-or-create-repl-buffer))
                   (unless (eq (current-buffer) (window-buffer))
                     (let* ((window (get-buffer-window (current-buffer)
                                                       t)))
                       (if window
                           (progn
                             (select-frame-set-input-focus (window-frame window))
                             (select-window window))
                         (display-buffer-pop-up-frame (current-buffer) '()))))
                   (goto-char (point-max))
                   (unless (null text)
                     (when nomis/cider-send-to-buffer-print-newline-first-p
                       (newline))
                     (when nomis/cider-send-to-buffer-do-return-first-p
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
    (let ((change-namespace-p
           (and (not nomis/cider-send-to-repl-always-p)
                (not (null (nomis/clojure-buffer-ns))) ; maybe this is always non-nil
                (not (equal (nomis/clojure-buffer-ns)
                            (nomis/cider-repl-namespace)))
                ;; (y-or-n-p
                ;;                (format "Buffer ns (%s) and REPL window ns (%s) are different.
                ;; Do you want to change the REPL window's namespace? (c-G to abort)"
                ;;                        (nomis/clojure-buffer-ns)
                ;;                        (nomis/cider-repl-namespace)))
                )))
      (when change-namespace-p
        (let* ((in-ns-text (s-concat "(in-ns '"
                                     (nomis/clojure-buffer-ns)
                                     ")")))
          (show-cider-repl-buffer-and-send-text-to-it in-ns-text))
        (sleep-for 0.5) ; is there a better way?
        ))
    (show-cider-repl-buffer-and-send-text-to-it (the-text))))

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
;;;; ---- Have `cider-find-var`, `cider-find-ns` and similar always re-use
;;;;      the selected window. ----

(cond
 ((member (nomis/cider-version)
          '("CIDER 0.9.1"))
  (defun cider-jump-to (buffer &optional pos other-window)
    "Push current point onto marker ring, and jump to BUFFER and POS.
POS can be either a numeric position in BUFFER or a cons (LINE . COLUMN)
where COLUMN can be nil. If OTHER-WINDOW is non-nil don't reuse current
window."
    (ring-insert find-tag-marker-ring (point-marker))
    (if other-window
        (pop-to-buffer buffer)
      ;; like switch-to-buffer, but reuse existing window if BUFFER is visible
      ;; jsk: change `pop-to-buffer` to `switch-to-buffer`
      (switch-to-buffer buffer '((display-buffer-reuse-window display-buffer-same-window))))
    (with-current-buffer buffer
      (widen)
      (goto-char (point-min))
      (cider-mode +1)
      (if (consp pos)
          (progn
            (forward-line (1- (or (car pos) 1)))
            (if (cdr pos)
                (move-to-column (cdr pos))
              (back-to-indentation)))
        (when pos
          (goto-char pos))))))
 ((or (member (nomis/cider-version)
              '("CIDER 0.10.0"
                "CIDER 0.12.0 (Seattle)"
                "CIDER 0.14.0 (Berlin)"
                "CIDER 0.15.0 (London)"
                "CIDER 0.16.0 (Riga)"
                "CIDER 0.17.0 (Andalucía)"
                "CIDER 0.18.0 (Saigon)"
                "CIDER 0.18.1snapshot"
                "CIDER 0.19.0snapshot"
                "CIDER 0.21.0 (New York)"
                "CIDER 0.22.0snapshot"
                "CIDER 0.22.0 (São Paulo)"
                "CIDER 0.22.1snapshot"
                "CIDER 0.23.0 (Lima)"))
      (and (equal (nomis/cider-version)
                  "CIDER 0.24.0snapshot")
           ;; Check whether fix is in CIDER itself.
           (not (boundp 'cider-jump-to-pop-to-buffer-actions))))
  (defvar *nomis/cider-jump-to/reuse-selected-window?* nil)
  (advice-add 'cider-jump-to
              :around
              (lambda (orig-fun &rest args)
                (let* ((*nomis/cider-jump-to/reuse-selected-window?* t))
                  (apply orig-fun args)))
              '((name . nomis/cider/hack-jump-to)))
  (advice-add 'pop-to-buffer
              :around
              (lambda (orig-fun buffer &rest other-args)
                (if *nomis/cider-jump-to/reuse-selected-window?*
                    (cl-case 2
                      ;; See https://github.com/clojure-emacs/cider/issues/2499
                      ;; Either of these should work.
                      (1 (switch-to-buffer buffer nil t))
                      (2 (funcall orig-fun
                                  buffer
                                  '((display-buffer-same-window)))))
                  (apply orig-fun buffer other-args)))
              '((name . nomis/cider/hack-jump-to))))
 ((boundp 'cider-jump-to-pop-to-buffer-actions)
  ;; We have the fix in CIDER itself.
  )
 (t
  (message-box
   "You need to fix `cider-jump-to` for this version of Cider.")))

(when nil ; Code to remove advice when in dev.
  (progn
    (advice-remove 'pop-to-buffer 'nomis/cider/hack-jump-to)
    (advice-remove 'cider-jump-to 'nomis/cider/hack-jump-to))
  )

;;;; ___________________________________________________________________________

(defvar nomis/cider-cljs-offer-to-open-app-in-browser? t
  "Obsolete. Use `cider-offer-to-open-cljs-app-in-browser` instead.")

(cond
 ((boundp 'cider-offer-to-open-cljs-app-in-browser)
  (advice-add
   'cider--offer-to-open-app-in-browser
   :around
   (lambda (orig-fun &rest args)
     (cl-flet ((do-it () (apply orig-fun args)))
       ;; We only get here when `cider-offer-to-open-cljs-app-in-browser` is
       ;; non-null.
       (cl-assert cider-offer-to-open-cljs-app-in-browser
               nil
               "`cider-offer-to-open-cljs-app-in-browser` is unexpectedly true")
       ;; Tell user to use CIDER's built-in approach.
       (when (or (local-variable-p
                  'nomis/cider-cljs-offer-to-open-app-in-browser?)
                 (null nomis/cider-cljs-offer-to-open-app-in-browser?))
         (beep)
         (message "Please use `cider-offer-to-open-cljs-app-in-browser` instead of `nomis/cider-cljs-offer-to-open-app-in-browser?`"))
       ;; Do what the user asked.
       (when nomis/cider-cljs-offer-to-open-app-in-browser?
         (do-it))))
   '((name . nomis/no-longer-need-nomis-hack))))

 ((or (member (nomis/cider-version)
              '("CIDER 0.22.1snapshot"
                "CIDER 0.23.0 (Lima)"
                "CIDER 0.24.0snapshot"
                "CIDER 0.26.1 (Nesebar)"))
      (member (pkg-info-version-info 'cider)
              '("1.2.0snapshot (package: 20210909.1011)")))
  (advice-add
   'cider--offer-to-open-app-in-browser
   :around
   (lambda (orig-fun &rest args)
     (when nomis/cider-cljs-offer-to-open-app-in-browser?
       (apply orig-fun args)))
   '((name . nomis/maybe-do-not-offer-to-open-app-in-browser))))

 (t
  (message-box
   "You need to fix `cider--offer-to-open-app-in-browser` for this version of Cider.")))

;;;; ___________________________________________________________________________

(provide 'nomis-cider-extras-non-lexical)
