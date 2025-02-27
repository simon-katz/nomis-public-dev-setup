;;;; nomis-eldoc.el --- Eldoc tailoring ---  -*- lexical-binding: t -*-

;;;; ___________________________________________________________________________

(use-package flycheck

  ;; This does the following:
  ;; - Sets `eldoc-documentation-strategy` to combine all Eldoc functions.
  ;; - Stops Eldoc and Flycheck blatting each others messages by:
  ;;   - Making Flycheck not display its messages.
  ;;   - Including Flycheck messages in a newly-written Eldoc function.
  ;;
  ;; Copied and modified from
  ;; https://www.masteringemacs.org/article/seamlessly-merge-multiple-documentation-sources-eldoc

  :preface

  (defun mp-flycheck-eldoc (callback &rest _ignored)
    "Print flycheck messages at point by calling CALLBACK."
    (when-let ((flycheck-errors (and flycheck-mode (flycheck-overlay-errors-at (point)))))
      (mapc
       (lambda (err)
         (funcall callback
                  (format "%s: [%s %s] %s"
                          (let ((level (flycheck-error-level err)))
                            (pcase level
                              ('info (propertize "I" 'face 'flycheck-error-list-info))
                              ('error (propertize "E" 'face 'flycheck-error-list-error))
                              ('warning (propertize "W" 'face 'flycheck-error-list-warning))
                              (_ level)))
                          (symbol-name (flycheck-error-checker err))
                          (or (flycheck-error-id err)
                              (flycheck-error-group err))
                          (flycheck-error-message err))
                  ;; :thing (or (flycheck-error-id err)
                  ;;            (flycheck-error-group err))
                  :face 'font-lock-doc-face))
       flycheck-errors)))

  (defun mp-flycheck-prefer-eldoc ()
    (add-hook 'eldoc-documentation-functions #'mp-flycheck-eldoc nil t)
    (setq eldoc-documentation-strategy 'eldoc-documentation-compose-eagerly)
    (setq flycheck-display-errors-function nil)
    ;; (setq flycheck-help-echo-function nil)
    )

  :hook ((flycheck-mode . mp-flycheck-prefer-eldoc)))

;;;; ___________________________________________________________________________
;;;; Add a prefix to all Eldoc messages to say which function produced it.
;;;; :possible-open-source-contribution Add prefix to Eldoc messages.

(defvar -nomis/eldoc-function-name-alist
  '((cider-eldoc        . "CIDER")
    (lsp-eldoc-function . "lsp")
    (mp-flycheck-eldoc  . "Flycheck")))

(defvar -nomis/eldoc-function-name-max-size
  (apply #'max
         (mapcar (-compose #'length #'cdr)
                 -nomis/eldoc-function-name-alist)))

(defun -nomis/eldoc-documentation-function->ui-name (f)
  (or (cdr (assoc f -nomis/eldoc-function-name-alist))
      (format "%s" f)))

(defun -nomis/eldoc-wrap-callback (f callback)
  (lambda (str &rest plist)
    (let ((f-name (-nomis/eldoc-documentation-function->ui-name f)))
      (apply callback
             (if (null str)
                 str
               (format "[%s]%s %s"
                       f-name
                       (make-string (max 0
                                         (- -nomis/eldoc-function-name-max-size
                                            (length f-name)))
                                    ?\s)
                       str))
             plist))))

(with-eval-after-load 'eldoc ; hack `nomis/eldoc-show-documentation-function`
  (cond
   ((member (pkg-info-version-info 'eldoc)
            '("1.13.0"))
    ;; The original `eldoc--documentation-compose-1` is in the `eldoc` package.
    (defun eldoc--documentation-compose-1 (eagerlyp)
      "Helper function for composing multiple doc strings.
If EAGERLYP is non-nil show documentation as soon as possible,
else wait for all doc strings."
      (run-hook-wrapped 'eldoc-documentation-functions
                        (lambda (f)
                          (let* ((callback (eldoc--make-callback
                                            (if eagerlyp :eager :patient)))
                                 (callback ; :nomis-hack
                                  (-nomis/eldoc-wrap-callback f callback))
                                 (str (funcall f callback)))
                            (if (or (null str) (stringp str)) (funcall callback str))
                            nil)))
      t))

   ((member (pkg-info-version-info 'eldoc)
            '("1.15.0"))
    ;; The original `eldoc-documentation-compose-eagerly` is in the `eldoc` package.
    (defun eldoc-documentation-compose-eagerly ()
      "Show multiple documentation strings one by one as soon as possible.
This is meant to be used as a value for `eldoc-documentation-strategy'."
      (run-hook-wrapped 'eldoc-documentation-functions
                        (lambda (f)
                          (let* ((callback (eldoc--make-callback :eager f))
                                 (callback ; :nomis-hack
                                  (-nomis/eldoc-wrap-callback f callback))
                                 (str (funcall f callback)))
                            (if (or (null str) (stringp str)) (funcall callback str))
                            nil)))
      t))

   (t
    (message-box
     "You need to fix `nomis/eldoc-show-documentation-function` for this version of `lsp-mode`."))))

;;;; ___________________________________________________________________________

(with-eval-after-load 'eldoc

  ;; From
  ;; https://www.masteringemacs.org/article/seamlessly-merge-multiple-documentation-sources-eldoc

  ;; For `M-x eldoc-doc-buffer`, open `*eldoc*` buffer at bottom of frame in
  ;; a fresh window.
  (add-to-list 'display-buffer-alist
               '("^\\*eldoc\\*"
                 display-buffer-at-bottom
                 (window-height . 4)))

  ;; Run Eldoc after more commands:
  (eldoc-add-command-completions "paredit-"))

;;;; ___________________________________________________________________________

(provide 'nomis-eldoc)
