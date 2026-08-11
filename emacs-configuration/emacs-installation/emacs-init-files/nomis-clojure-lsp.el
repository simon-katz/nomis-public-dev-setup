;;; nomis-clojure-lsp.el --- Clojure LSP setup -*- lexical-binding: t; -*-

;;;; See https://emacs-lsp.github.io/lsp-mode/tutorials/clojure-guide/

;;;; ___________________________________________________________________________

(require 'nomis-lsp)
(require 'nomis-re-frame-jump)

;;;; ___________________________________________________________________________

(add-hook 'clojure-mode-hook 'lsp)
;; Might also want (see https://emacs-lsp.github.io/lsp-mode/tutorials/clojure-guide/):
;; (add-hook 'clojurescript-mode-hook 'lsp)
;; (add-hook 'clojurec-mode-hook 'lsp)

;;;; ___________________________________________________________________________

(defun nomis/clojure-lsp-init ()
  (setq lsp-clojure-workspace-dir
        (expand-file-name "~/.emacs-d-stuff/lsp-clojure-workspace"))
  (setq lsp-clojure-workspace-cache-dir
        (expand-file-name "~/.emacs-d-stuff/lsp-clojure-workspace/cache")))

(add-hook 'lsp-mode-hook 'nomis/clojure-lsp-init)

;;;; ___________________________________________________________________________
;;;; Keep track of xref pushes

(defvar nomis/-clojure-lsp/xref-push-tracked? nil)

(defun nomis/-clojure-lsp/start-tracking-xref-push ()
  (setq nomis/-clojure-lsp/xref-push-tracked? nil)
  (advice-add 'xref-push-marker-stack
              :before
              (lambda (&rest _) (setq nomis/-clojure-lsp/xref-push-tracked? t))
              '((name . nomis/-clojure-lsp/track-xref-push))))

(defun nomis/-clojure-lsp/stop-tracking-xref-push ()
  (advice-remove 'xref-push-marker-stack 'nomis/-clojure-lsp/track-xref-push))

;;;; ___________________________________________________________________________
;;;; ---- nomis/clojure-lsp-and-cider/find-definition ----

;; We had commented this out because `cider-xref-fn-depth` has been introduced
;; (meaning CIDER is used in preference to LSP), but we've re-introduced it
;; because LSP lets us find clojure.spec definitions for symbols.

(defvar nomis/clojure-lsp-and-cider/find-definition/use-lsp? nil)

(defun nomis/clojure-lsp-and-cider/find-definition ()
  "Try to find definition of thing at point.
 If the thing at point is a keyword, use LSP. If the thing at
 point is a not a keyword, try using CIDER; if no luck with
 CIDER, use LSP."
  (interactive)
  (if (or nomis/clojure-lsp-and-cider/find-definition/use-lsp?
          (ignore-errors (s-starts-with? ":" (symbol-name (symbol-at-point)))))
      (lsp-find-definition)
    (cl-flet ((buffer-and-point () (list (current-buffer) (point))))
      (let* ((old-bap (buffer-and-point)))
        (when (cider-repls)
          (cider-find-var))
        (let* ((new-bap (buffer-and-point)))
          (when (equal old-bap new-bap)
            (beep)
            (message "Couldn't find definition using CIDER -- trying LSP")
            (lsp-find-definition)))))))

(defun nomis/clojure-lsp-and-cider/find-definition-v2 ()
  "Try to find definition of thing at point.
 First try lsp; if that gives an error try CIDER."
  (interactive)
  (cl-flet ((buffer-and-point () (list (current-buffer) (point))))
    (let* ((initial-bap (buffer-and-point)))
      (unwind-protect
          ;; Try lsp:
          (lsp-find-definition)
        (when (equal initial-bap (buffer-and-point))
          ;; lsp didn't find the definition, so try CIDER:
          (when (cider-repls)
            ;; Use `run-at-time` so that any lsp-produced exception is dealt
            ;; with first.
            (run-at-time 0 nil (lambda () (cider-find-var nil)))))))))

(defconst nomis/-clojure-lsp/debug? nil)

(defun nomis/clojure-lsp-and-cider/find-definition-v3 ()
  "Try to find definition of thing at point.
- First try lsp.
- Then, if on a keyword, try `nomis/re-frame-jump-to-reg'.
- Then try `cider-find-var`.
We leave `cider-find-var` to the end in case it does async stuff. (I think it
doesn't, FWIW)."
  (interactive)
  (cl-flet ((buffer-and-point () (list (current-buffer) (point)))
            (debug-message (format-string &rest args)
              (when nomis/-clojure-lsp/debug?
                (let* ((inhibit-message t))
                  (apply #'message format-string args)))))
    (let* ((initial-bap  (buffer-and-point))
           (initial-line (line-number-at-pos))
           (sym          (cider-symbol-at-point 'look-back))
           (at-keyword?  (and sym (string-prefix-p ":" sym))))
      (nomis/-clojure-lsp/start-tracking-xref-push)
      (unwind-protect
          (lsp-find-definition)
        (nomis/-clojure-lsp/stop-tracking-xref-push)
        (let* ((lsp-kw-fail?
                ;; When lsp "fails" on keywords, it moves to the start of the
                ;; keyword (same buffer, same line) rather than to
                ;; a real definition.
                (and at-keyword?
                     (eq (car initial-bap) (current-buffer))
                     (= initial-line (line-number-at-pos))))
               (lsp-found? (and (not lsp-kw-fail?)
                                (not (equal initial-bap (buffer-and-point))))))
          (if lsp-found?
              (debug-message "Found by lsp")
            (when (and lsp-kw-fail?
                       nomis/-clojure-lsp/xref-push-tracked?)
              ;; Restore original position.
              (let* ((xref-after-return-hook (remove 'xref-pulse-momentarily
                                                     xref-after-return-hook)))
                (xref-go-back)))
            (let* ((re-frame-found?
                    ;; Note that lsp finds re-frame definitions that use
                    ;; non-alpha defining operators, so we only need this for
                    ;; the alpha ones. - Simon Katz 2026-08-11
                    (and at-keyword?
                         (nomis/re-frame-jump-to-reg))))
              (if re-frame-found?
                  (debug-message "Found by nomis/re-frame-jump-to-reg")
                (when (cider-repls)
                  (debug-message "Trying cider-find-var")
                  ;; Use `run-at-time` so that any lsp-produced exception is
                  ;; dealt with first.
                  (run-at-time 0 nil (lambda () (cider-find-var nil))))))))))))

(with-eval-after-load 'cider
  (with-eval-after-load 'lsp-mode
    (cond
     ((member (list (pkg-info-package-version 'cider)
                    (pkg-info-package-version 'lsp-mode) ; Hmmm, I guess you really want to depend on the version of `clojure-lsp` executable installed on the system.
                    )
              '(((20210909 1011) (20210821 1359))
                ((20210929 1032) (20210821 1359))
                ((20211105 708)  (20211103 1331))
                ((20220405 1216) (20211103 1331))
                ((20220830 500)  (20211103 1331))))

      (dolist (m (list clojure-mode-map
                       cider-mode-map
                       clojurec-mode-map
                       clojurescript-mode-map))
        (define-key m (kbd "M-.") 'nomis/clojure-lsp-and-cider/find-definition)))

     ((and (version-list-<= '(20230518 55)
                            (pkg-info-package-version 'cider))
           (version-list-<= (pkg-info-package-version 'cider)
                            '(20250429 0)))
      ;; Works now (it finds a Clojure Spec keyword).
      )

     (t
      (dolist (m (list clojure-mode-map
                       cider-mode-map
                       clojurec-mode-map
                       clojurescript-mode-map))
        (define-key m (kbd "M-.") 'nomis/clojure-lsp-and-cider/find-definition-v3))))))

;;;; ___________________________________________________________________________

(provide 'nomis-clojure-lsp)
