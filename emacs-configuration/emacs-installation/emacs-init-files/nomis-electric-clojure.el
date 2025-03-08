;;;; nomis-electric-clojure.el --- Minor mode for Electric Clojure ---  -*- lexical-binding: t -*-

;;;; Inspired by
;;;; https://gitlab.com/xificurC/hf-electric.el/-/blob/master/hf-electric.el
;;;; Permalink: https://gitlab.com/xificurC/hf-electric.el/-/blob/5e6e3d69e42a64869f1eecd8b804cf4b679f9501/hf-electric.el

;;;; TODO: See all TODOs.

;;;; TODO: Fix v3 being initially sited on the client.
;;;;       Make it work for all uses of `e/defn`:
;;;;       - Doc strings.
;;;;       - Attribute map.
;;;;       - Multiple arities.

;;;; TODO: Where is v2 initially sited? Seems to be on the server. But there's
;;;;       code to run it on both server and client. Maybe make it neutral.
;;;;       Maybe ask on Slack (after mentioning this minor mode.)

;;;; TODO: Look at retaining/reapplying this minor mode when reverting buffer.
;;;;       - Especially because of auto-revert when a file changes.

;;;; TODO: How is this on a big file?
;;;;       Not good. Simple editing can cause never-returning ~90% CPU.
;;;;       - Maybe fixed.

;;;; TODO: Check for no dependencies on other packages.
;;;;       - I've checked that there's no use of:
;;;;         - `dash`
;;;;         - `nomis-sexp-utils`.

;;;; TODO: Decide how to publish. I think just point at this file.

;;;; ___________________________________________________________________________

(defcustom nomis/ec-bound-for-electric-require-search 100000
  "How far to search in Electric Clojure source code buffers when
trying to detect the version of Electric Clojure.

This detection is done by looking for
  `[hyperfiddle.electric :as e]`
or
  `[hyperfiddle.electric3 :as e]`
near the beginning of the buffer.

We normally do this once for each buffer.

You can re-run the auto-detection in either of the following ways:
- by running `M-x nomis/ec-redetect-electric-version`
- by turning `nomis-electric-clojure-mode` off and then back on."
  :type 'integer)

(defvar nomis/ec-highlight-initial-whitespace? nil) ; TODO: Change to `defcustom`.

(defface nomis/ec-client-face
  `((((background dark)) ,(list :background "DarkGreen"))
    (t ,(list :background "DarkSeaGreen1")))
  "Face for Electric Clojure client code.")

(defface nomis/ec-server-face
  `((((background dark)) ,(list :background "IndianRed4"))
    (t ,(list :background "#ffc5c5")))
  "Face for Electric Clojure server code.")

(defface nomis/ec-neutral-face
  `((t ,(list :inherit 'default)))
  "Face for Electric code that is not specifically client code nor
specifically server code.

This can be:
- code that is either client or server code; for example:
  - code that is not lexically within `e/client` or `e/server`
  - an `(e/fn ...)`
- code that is neither client nor server; for example:
  - in Electric v3:
    - symbols that are being bound; /eg/ the LHS of `let` bindings
    - `dom/xxxx` symbols.")

;;;; ___________________________________________________________________________

(defvar -nomis/ec-electric-version nil)
(make-variable-buffer-local '-nomis/ec-electric-version)

;;;; ___________________________________________________________________________

(defun nomis/ec-message-no-disp (format-string &rest args)
  (let* ((inhibit-message t))
    (apply #'message format-string args)))

;;;; ___________________________________________________________________________
;;;; Some utilities copied from `nomis-sexp-utils`. (I don't want to
;;;; make this package dependent on `nomis-sexp-utils`.)

(defvar -nomis/ec-regexp-for-bracketed-sexp-start
  "(\\|\\[\\|{\\|#{")

(defun -nomis/ec-looking-at-bracketed-sexp-start ()
  (looking-at -nomis/ec-regexp-for-bracketed-sexp-start))

(defun -nomis/ec-at-top-level? ()
  (save-excursion
    (condition-case nil
        (progn (backward-up-list) nil)
      (error t))))

(defun -nomis/ec-forward-sexp-gives-no-error? ()
  (save-excursion
    (condition-case nil
        (progn (forward-sexp) t)
      (error nil))))

(defun -nomis/ec-can-forward-sexp? ()
  ;; This is complicated, because `forward-sexp` behaves differently at end
  ;; of file and inside-and-at-end-of a `(...)` form.
  (cond ((not (-nomis/ec-at-top-level?))
         (-nomis/ec-forward-sexp-gives-no-error?))
        ((and (thing-at-point 'symbol)
              (save-excursion (ignore-errors (forward-char) t))
              (save-excursion (forward-char) (thing-at-point 'symbol)))
         ;; We're on a top-level symbol (and not after its end).
         t)
        (t
         (or (bobp) ; should really check that there's an sexp ahead
             (condition-case nil
                 (not (= (save-excursion
                           (backward-sexp)
                           (point))
                         (save-excursion
                           (forward-sexp)
                           (backward-sexp)
                           (point))))
               (error nil))))))

;;;; ___________________________________________________________________________
;;;; Flashing of the re-overlayed region, to help with debugging.

(defvar -nomis/ec-give-debug-feedback-flash? nil) ; for debugging

(defface -nomis/ec-flash-update-region-face-1
  `((t ,(list :background "red3")))
  "Face for Electric Clojure flashing of provided region.")

(defface -nomis/ec-flash-update-region-face-2
  `((t ,(list :background "yellow")))
  "Face for Electric Clojure flashing of extended region.")

(defun -nomis/ec-feedback-flash (start end start-2 end-2)
  (when -nomis/ec-give-debug-feedback-flash?
    (let* ((flash-overlay-1
            (let* ((ov (make-overlay start end nil t nil)))
              (overlay-put ov 'category 'nomis/ec-overlay)
              (overlay-put ov 'face '-nomis/ec-flash-update-region-face-1)
              (overlay-put ov 'evaporate t)
              (overlay-put ov 'priority 999999)
              ov))
           (flash-overlay-2
            (let* ((ov (make-overlay start-2 end-2 nil t nil)))
              (overlay-put ov 'category 'nomis/ec-overlay)
              (overlay-put ov 'face '-nomis/ec-flash-update-region-face-2)
              (overlay-put ov 'evaporate t)
              (overlay-put ov 'priority 999999)
              ov)))
      (run-at-time 0.2
                   nil
                   (lambda ()
                     (delete-overlay flash-overlay-1)
                     (delete-overlay flash-overlay-2))))))

;;;; ___________________________________________________________________________

(defvar *-nomis/ec-n-lumps-in-current-update*)

(defvar *-nomis/ec-site* :neutral
  "The site of the code currently being analysed. One of `:neutral`,
`:client` or `:server`.")

(defvar *-nomis/ec-level* 0)

;;;; ___________________________________________________________________________
;;;; Overlay basics

(defun -nomis/ec-make-overlay (nesting-level face start end)
  (let* ((ov (make-overlay start end nil t nil)))
    (overlay-put ov 'category 'nomis/ec-overlay)
    (overlay-put ov 'face face)
    (overlay-put ov 'evaporate t)
    (unless nomis/ec-highlight-initial-whitespace?
      ;; We have multiple overlays in the same place, so we need to
      ;; specify their priority.
      (overlay-put ov 'priority (cons nil nesting-level)))
    ov))

(defun -nomis/ec-overlay-single-lump (site nesting-level start end)
  (cl-incf *-nomis/ec-n-lumps-in-current-update*)
  (let* ((face (cl-case site
                 (:client  'nomis/ec-client-face)
                 (:server  'nomis/ec-server-face)
                 (:neutral 'nomis/ec-neutral-face))))
    (if nomis/ec-highlight-initial-whitespace?
        (-nomis/ec-make-overlay nesting-level face start end)
      (save-excursion
        (while (< (point) end)
          (let* ((start-2 (point))
                 (end-2 (min end
                             (progn (end-of-line) (1+ (point))))))
            (-nomis/ec-make-overlay nesting-level face start-2 end-2)
            (unless (eobp) (forward-char))
            (when (bolp)
              (back-to-indentation))))))))

;;;; ___________________________________________________________________________
;;;; ---- Parse and overlay helpers ----

(defun -nomis/ec-checking-movement* (desc move-fn overlay-fn)
  (condition-case _
      (funcall move-fn)
    (error (nomis/ec-message-no-disp
            "nomis-electric-clojure: Failed to parse %s"
            desc)))
  (funcall overlay-fn))

(cl-defmacro -nomis/ec-checking-movement ((desc move-form) &body body)
  (declare (indent 1))
  `(-nomis/ec-checking-movement* ,desc
                                 (lambda () ,move-form)
                                 (lambda () ,@body)))

(defun -nomis/ec-bof ()
  (forward-sexp)
  (backward-sexp))

(defun -nomis/ec-with-site* (site end f)
  (let* ((start (point))
         (end (or end
                  (save-excursion (forward-sexp) (point))))
         (*-nomis/ec-level* (1+ *-nomis/ec-level*)))
    (if (eq site *-nomis/ec-site*)
        ;; No need for a new overlay.
        (funcall f)
      (let* ((*-nomis/ec-site* site))
        (-nomis/ec-overlay-single-lump site *-nomis/ec-level* start end)
        (funcall f)))))

(cl-defmacro -nomis/ec-with-site ((site &optional end) &body body)
  (declare (indent 1))
  `(-nomis/ec-with-site* ,site ,end (lambda () ,@body)))

(defun -nomis/ec-overlay-args-of-form ()
  (save-excursion
    (down-list)
    (forward-sexp)
    (while (-nomis/ec-can-forward-sexp?)
      (-nomis/ec-bof)
      (-nomis/ec-walk-and-overlay)
      (forward-sexp))))

(defun -nomis/ec-overlay-site (site)
  (save-excursion
    (-nomis/ec-with-site (site)
      (-nomis/ec-overlay-args-of-form))))

(defun -nomis/ec-overlay-body (site)
  (save-excursion
    (when (-nomis/ec-can-forward-sexp?)
      (-nomis/ec-bof)
      ;; Whole body:
      (-nomis/ec-with-site (site
                            (let ((body-end
                                   (save-excursion (backward-up-list)
                                                   (forward-sexp)
                                                   (backward-char)
                                                   (point))))
                              body-end))
        ;; Each body form:
        (while (-nomis/ec-can-forward-sexp?)
          (-nomis/ec-bof)
          (-nomis/ec-walk-and-overlay)
          (forward-sexp))))))

;;;; ___________________________________________________________________________
;;;; ---- Parse and overlay ----

(defun -nomis/ec-overlay-defn ()
  (save-excursion
    ;; Body:
    (-nomis/ec-checking-movement ("e/defn"
                                  (progn (down-list) (forward-sexp 3)))
      (-nomis/ec-overlay-body (cl-case -nomis/ec-electric-version
                                (:v2 :server) ; See "Where is v2 initially sited?" question at top of file.
                                (:v3 :client))))))

(defun -nomis/ec-overlay-dom-xxxx ()
  (save-excursion
    (save-excursion (down-list)
                    (-nomis/ec-with-site (:client)
                      ;; Nothing more.
                      ))
    (-nomis/ec-overlay-args-of-form)))

(defun -nomis/ec-overlay-let ()
  (save-excursion
    (let* ((inherited-site *-nomis/ec-site*))
      ;; Whole form:
      (-nomis/ec-with-site (:neutral)
        ;; Bindings:
        (-nomis/ec-checking-movement ("let"
                                      (down-list 2))
          (while (-nomis/ec-can-forward-sexp?)
            ;; Skip the LHS of the binding:
            (forward-sexp)
            ;; Walk the RHS of the binding, if there is one:
            (when (-nomis/ec-can-forward-sexp?)
              (-nomis/ec-bof)
              (-nomis/ec-with-site (inherited-site)
                (-nomis/ec-walk-and-overlay))
              (forward-sexp))))
        ;; Body:
        (backward-up-list)
        (forward-sexp)
        (-nomis/ec-overlay-body inherited-site)))))

(defun -nomis/ec-overlay-other-bracketed-form ()
  (save-excursion
    (down-list)
    (while (-nomis/ec-can-forward-sexp?)
      (-nomis/ec-bof)
      (-nomis/ec-walk-and-overlay)
      (forward-sexp))))

(defun -nomis/ec-walk-and-overlay ()
  (save-excursion
    (cl-case -nomis/ec-electric-version
      (:v2
       (cond ((looking-at "(e/defn\\_>")
              (-nomis/ec-overlay-defn))
             ((looking-at "(e/client\\_>")
              (-nomis/ec-overlay-site :client))
             ((looking-at "(e/server\\_>")
              (-nomis/ec-overlay-site :server))
             ((-nomis/ec-looking-at-bracketed-sexp-start)
              (-nomis/ec-overlay-other-bracketed-form))))
      (:v3
       (cond ((looking-at "(e/defn\\_>")
              (-nomis/ec-overlay-defn))
             ((looking-at "(e/client\\_>")
              (-nomis/ec-overlay-site :client))
             ((looking-at "(e/server\\_>")
              (-nomis/ec-overlay-site :server))
             ((looking-at "(e/fn\\_>")
              (-nomis/ec-overlay-site :neutral))
             ((looking-at "(dom/")
              (-nomis/ec-overlay-dom-xxxx))
             ((looking-at "(let\\_>")
              (-nomis/ec-overlay-let))
             ((-nomis/ec-looking-at-bracketed-sexp-start)
              (-nomis/ec-overlay-other-bracketed-form)))))))

(defun -nomis/ec-buffer-has-text? (s)
  (save-excursion (goto-char 0)
                  (search-forward s
                                  nomis/ec-bound-for-electric-require-search
                                  t)))

(defun -nomis/ec-detect-electric-version ()
  (let* ((v (cond ((-nomis/ec-buffer-has-text? "[hyperfiddle.electric3 :as e]")
                   :v3)
                  ((-nomis/ec-buffer-has-text? "[hyperfiddle.electric :as e]")
                   :v2)
                  (t
                   :v3))))
    (setq -nomis/ec-electric-version v)
    (message "Electric version = %s"
             (string-replace ":" "" (symbol-name v)))))

(defun -nomis/ec-overlay-region (start end)
  (unless -nomis/ec-electric-version
    (-nomis/ec-detect-electric-version))
  (let* ((*-nomis/ec-n-lumps-in-current-update* 0))
    (save-excursion
      (goto-char start)
      (unless (-nomis/ec-at-top-level?) (beginning-of-defun))
      (let* ((start-2 (point))
             (end-2 (save-excursion (goto-char end)
                                    (unless (-nomis/ec-at-top-level?)
                                      (end-of-defun))
                                    (point))))
        (remove-overlays start-2 end-2 'category 'nomis/ec-overlay)
        (while (and (< (point) end-2)
                    (-nomis/ec-can-forward-sexp?))
          (-nomis/ec-bof)
          (-nomis/ec-walk-and-overlay)
          (forward-sexp))
        (-nomis/ec-feedback-flash start end start-2 end-2)
        `(jit-lock-bounds ,start-2 . ,end-2)))
    ;; (nomis/ec-message-no-disp "*-nomis/ec-n-lumps-in-current-update* = %s"
    ;;                           *-nomis/ec-n-lumps-in-current-update*)
    ))

;;;; ___________________________________________________________________________

(defun -nomis/ec-turn-on ()
  (jit-lock-register '-nomis/ec-overlay-region t))

(defun -nomis/ec-turn-off ()
  (setq -nomis/ec-electric-version nil) ; so we will re-detect this
  (jit-lock-unregister '-nomis/ec-overlay-region)
  (remove-overlays nil nil 'category 'nomis/ec-overlay))

(defun -nomis/ec-before-revert ()
  (-nomis/ec-turn-off))

(define-minor-mode nomis-electric-clojure-mode
  "Highlight Electric Clojure client code regions and server code regions."
  :init-value nil
  (if nomis-electric-clojure-mode
      (progn
        (-nomis/ec-turn-on)
        (add-hook 'before-revert-hook '-nomis/ec-before-revert nil t))
    (progn
      (-nomis/ec-turn-off)
      (remove-hook 'before-revert-hook '-nomis/ec-before-revert t))))

;;;; ___________________________________________________________________________
;;;; ---- nomis/ec-redetect-electric-version ----

(defun nomis/ec-redetect-electric-version ()
  (interactive)
  (cl-assert nomis-electric-clojure-mode)
  (-nomis/ec-turn-off)
  (-nomis/ec-turn-on))

;;;; ___________________________________________________________________________
;;;; ---- nomis/ec-toggle-highlight-initial-whitespace? ----

(defun nomis/ec-toggle-highlight-initial-whitespace? ()
  (interactive)
  (if (not nomis-electric-clojure-mode)
      (nomis-electric-clojure-mode)
    (progn
      (setq nomis/ec-highlight-initial-whitespace?
            (not nomis/ec-highlight-initial-whitespace?))
      (-nomis/ec-overlay-region (point-min) (point-max)))))

;;;; ___________________________________________________________________________
;;;; ---- nomis/ec-report-overlays ----

(defun nomis/ec-report-overlays ()
  (interactive)
  (let* ((all-ovs (overlays-in (point-min) (point-max)))
         (ovs (cl-remove-if-not (lambda (ov)
                                  (eq 'nomis/ec-overlay
                                      (overlay-get ov 'category)))
                                all-ovs)))
    (message "----------------")
    (dolist (ov ovs)
      (let* ((ov-start (overlay-start ov))
             (ov-end   (overlay-end ov))
             (end      (min ov-end
                            (save-excursion
                              (goto-char ov-start)
                              (pos-eol)))))
        (nomis/ec-message-no-disp "%s %s %s%s"
                                  (overlay-get ov 'priority)
                                  ov
                                  (buffer-substring ov-start end)
                                  (if (> ov-end end)
                                      "..."
                                    ""))))
    (nomis/ec-message-no-disp "No. of overlays = %s" (length ovs))))

;;;; ___________________________________________________________________________

(provide 'nomis-electric-clojure)
