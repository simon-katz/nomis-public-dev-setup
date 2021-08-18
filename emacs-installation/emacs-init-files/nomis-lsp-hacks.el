;;; nomis-lsp-hacks.el --- LSP hacks -*- lexical-binding: t; -*-

;;;; ___________________________________________________________________________

(with-eval-after-load 'lsp-ui-sideline
  (cond
   ((member (pkg-info-package-version 'lsp-ui)
            '((20210802 305)))

    ;; There's a thing where long diagnostic messages aren't displayed. The code
    ;; here truncates long messages so that there is more chance of them being
    ;; displayed.
    ;;
    ;; The code that determines where to put messages and whether they will fit
    ;; is side-effecty. I had tried repeatedly trying things in a loop, each
    ;; time knocking a character off the end of the message until `pos-ov` is
    ;; non-nil, but the side effects meant that the messages would jump about in
    ;; a mad way as you typed (because each attempt caused the recording of a
    ;; position as being used for messages, I think.)
    ;;
    ;; So instead I have an approach that works only some of the time: simply
    ;; truncate long messages to the window width and hope that there's a place
    ;; they can be displayed.

    (defconst nomis/lsp-ui-sideline--truncation-suffix " ▶▶")
    (defconst nomis/lsp-ui-sideline--truncation-suffix-len
      (length nomis/lsp-ui-sideline--truncation-suffix))

    (defun nomis/lsp-ui-sideline--maybe-truncate (msg n)
      (let* ((suffix nomis/lsp-ui-sideline--truncation-suffix)
             (suffix-len nomis/lsp-ui-sideline--truncation-suffix-len)
             (len (length msg)))
        (cond ((<= len n)
               ;; No need to truncate.
               msg)
              ((< n suffix-len)
               ;; Not a good situation. What should we do?
               (apply #'concat (-repeat n "◼")))
              (t
               (concat (substring msg 0 (- n suffix-len))
                       suffix)))))

    ;; The original `lsp-ui-sideline--diagnostics` is in `lsp-ui-sideline`.
    (defun lsp-ui-sideline--diagnostics (buffer bol eol)
      "Show diagnostics belonging to the current line.
Loop over flycheck errors with `flycheck-overlay-errors-in'.
Find appropriate position for sideline overlays with `lsp-ui-sideline--find-line'.
Push sideline overlays on `lsp-ui-sideline--ovs'."
      (when (and (bound-and-true-p flycheck-mode)
                 (bound-and-true-p lsp-ui-sideline-mode)
                 lsp-ui-sideline-show-diagnostics
                 (eq (current-buffer) buffer))
        (lsp-ui-sideline--delete-kind 'diagnostics)
        (dolist (e (flycheck-overlay-errors-in bol (1+ eol)))
          (let* ((lines (--> (flycheck-error-format-message-and-id e)
                             (split-string it "\n")
                             (lsp-ui-sideline--split-long-lines it)))
                 (display-lines (butlast lines (- (length lines) lsp-ui-sideline-diagnostic-max-lines)))
                 (offset 1))
            (dolist (line (nreverse display-lines))
              (let* ((msg (string-trim (replace-regexp-in-string "[\t ]+" " " line)))
                     (msg (replace-regexp-in-string " " " " msg))
                     ;; nomis-hack: Without this, over-long messages are not
                     ;;             shown. This limits `msg` to the window
                     ;;             width.
                     (msg (let* ((window-width (1-
                                                ;; gives 1 too big for me
                                                (lsp-ui-sideline--window-width))))
                            (nomis/lsp-ui-sideline--maybe-truncate msg
                                                                   window-width)))
                     (len (length msg))
                     (level (flycheck-error-level e))
                     (face (if (eq level 'info) 'success level))
                     (margin (lsp-ui-sideline--margin-width))
                     (msg (progn (add-face-text-property 0 len 'lsp-ui-sideline-global nil msg)
                                 (add-face-text-property 0 len face nil msg)
                                 msg))
                     (string (concat (propertize " " 'display `(space :align-to (- right-fringe ,(lsp-ui-sideline--align len margin))))
                                     (propertize msg 'display (lsp-ui-sideline--compute-height))))
                     (pos-ov (lsp-ui-sideline--find-line len bol eol t offset))
                     (ov (and pos-ov (make-overlay (car pos-ov) (car pos-ov)))))
                (when pos-ov
                  (setq offset (1+ (car (cdr pos-ov))))
                  (overlay-put ov 'after-string string)
                  (overlay-put ov 'kind 'diagnostics)
                  (overlay-put ov 'before-string " ")
                  (overlay-put ov 'position (car pos-ov))
                  (push ov lsp-ui-sideline--ovs)))))))))

   (t
    (message-box
     "You need to fix `lsp-ui-sideline--diagnostics` for this version of lsp-ui-sideline."))))

;;;; ___________________________________________________________________________

(with-eval-after-load 'lsp-diagnostics ; hack `lsp-diagnostics--flycheck-level`

  ;; Get rid of huge numbers of messages like this:
  ;;     Invalid face reference: lsp-flycheck-info-unnecessary
  ;;     Invalid face reference: lsp-flycheck-warning-unnecessary
  ;;
  ;; See https://github.com/emacs-lsp/lsp-mode/issues/2255

  (cond
   ((member (pkg-info-package-version 'lsp-mode)
            '((20210808 2036)))

    ;; The original `lsp-diagnostics--flycheck-level` is in the
    ;; `lsp-diagnostics` package (but note that `M-.` on that takes you to a
    ;; function in `lsp-mode` so be careful).
    (defun lsp-diagnostics--flycheck-level (flycheck-level tags)
      "Generate flycheck level from the original FLYCHECK-LEVEL (e.
g. `error', `warning') and list of LSP TAGS."
      (let ((name (format "lsp-flycheck-%s-%s"
                          flycheck-level
                          (mapconcat #'symbol-name tags "-"))))
        (or (intern-soft name)
            (let* ((face (--doto (intern name)
                           (copy-face (-> flycheck-level
                                          (get 'flycheck-overlay-category)
                                          (get 'face))
                                      it)
                           (mapc (lambda (tag)
                                   (apply #'set-face-attribute it nil
                                          (cl-rest (assoc tag lsp-diagnostics-attributes))))
                                 tags)))
                   (category (--doto (intern (format "%s-category" name))
                               (setf (get it 'face) face
                                     (get it 'priority) 100)))
                   (new-level (intern name))
                   (bitmap (or (get flycheck-level 'flycheck-fringe-bitmaps)
                               (get flycheck-level 'flycheck-fringe-bitmap-double-arrow))))
              (flycheck-define-error-level new-level
                :severity (get flycheck-level 'flycheck-error-severity)
                :compilation-level (get flycheck-level 'flycheck-compilation-level)
                :overlay-category category
                :fringe-bitmap bitmap
                :fringe-face (get flycheck-level 'flycheck-fringe-face)
                :error-list-face face)
              new-level)))))

   (t
    (message-box
     "You need to fix `lsp-diagnostics--flycheck-level` for this version of lsp-ui-sideline."))))

;;;; ___________________________________________________________________________

(provide 'nomis-lsp-hacks)
