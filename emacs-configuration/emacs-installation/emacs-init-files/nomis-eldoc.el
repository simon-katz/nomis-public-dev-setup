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
                  (format "%s: %s"
                          (let ((level (flycheck-error-level err)))
                            (pcase level
                              ('info (propertize "I" 'face 'flycheck-error-list-info))
                              ('error (propertize "E" 'face 'flycheck-error-list-error))
                              ('warning (propertize "W" 'face 'flycheck-error-list-warning))
                              (_ level)))
                          (flycheck-error-message err))
                  :thing (or (flycheck-error-id err)
                             (flycheck-error-group err))
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

;;;; TODO: Can you find a way to add a prefix to all Eldoc messages (with the
;;;;       prefix determined by the function that produces the message) in
;;;;       a generic way? This would replace your hacks with
;;;;       `nomis/-cider-eldoc-message-prefix` and
;;;;       `nomis/-lsp-eldoc-message-prefix`.
;;;;
;;;;       You'd want to add prefixes for:
;;;;       - `lsp-eldoc-function`
;;;;       - `mp-flycheck-eldoc`
;;;;       - `cider-eldoc`
;;;;
;;;;       This is a :possible-open-source-contribution

;;;; ___________________________________________________________________________

(provide 'nomis-eldoc)
