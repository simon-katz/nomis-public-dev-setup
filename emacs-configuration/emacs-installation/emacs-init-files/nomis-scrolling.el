;;; nomis-scrolling.el --- Scrolling hacks  -*- lexical-binding: t; -*-

;;; Code:

;;;; Requires

(require 'cl-lib)

;;;; nomis/scrolling/-debug

(defvar nomis/scrolling/-debug? nil)

(defun nomis/scrolling/-debug (format-string &rest format-args)
  (let* ((inhibit-message t))
    (when nomis/scrolling/-debug?
      (apply #'message format-string format-args))))

;;;; maintain-line-no-in-window

(defvar nomis/scrolling/maintain-line-no-in-window? nil)

(defun nomis/scrolling/toggle-maintain-line-no-in-window ()
  (interactive)
  (message "nomis/scrolling/maintain-line-no-in-window? = %s"
           (setq nomis/scrolling/maintain-line-no-in-window?
                 (not nomis/scrolling/maintain-line-no-in-window?))))

(defun nomis/scrolling/-line-no-in-window ()
  (if (= (point) (window-start))
      ;; Bug in `count-screen-lines`? It's returning 0.
      1
    (count-screen-lines (window-start) (point) t)))

(defun nomis/scrolling/restore-scroll-position (old-line-no)
  ;; Ensure cursor is on screen, so that scrolling doesn't make
  ;; any unwanted adjustments.
  (let* ((recenter-redisplay nil))
    (recenter))
  ;; Reset scroll position.
  (ignore-errors
    ;; `ignore-errors` because if we're near the top of the buffer we may not be
    ;; able to do this.
    (scroll-up-line (- (nomis/scrolling/-line-no-in-window)
                       old-line-no))))

(defvar nomis/scrolling/-old-line-no nil)

(defun nomis/scrolling/maybe-restore-scroll-position ()
  "Unused. Was needed when we had a `run-at-time` before expanding parents."
  (when (and nomis/scrolling/maintain-line-no-in-window?
             nomis/scrolling/-old-line-no)
    (nomis/scrolling/restore-scroll-position nomis/scrolling/-old-line-no)))

(defun nomis/scrolling/-with-maybe-maintain-line-no-in-window* (fun force?)
  (cl-flet* ((do-it () (funcall fun)))
    (if (not (or force?
                 nomis/scrolling/maintain-line-no-in-window?))
        (progn (setq nomis/scrolling/-old-line-no nil)
               (do-it))
      (let* ((old-line-no (nomis/scrolling/-line-no-in-window)))
        (setq nomis/scrolling/-old-line-no old-line-no)
        (prog1 (do-it)
          (nomis/scrolling/restore-scroll-position old-line-no))))))

(cl-defmacro nomis/scrolling/with-maybe-maintain-line-no-in-window (&body body)
  (declare (indent 0))
  `(nomis/scrolling/-with-maybe-maintain-line-no-in-window* (lambda () ,@body)
                                                            nil))

(cl-defmacro nomis/scrolling/with-force-maintain-line-no-in-window (&body body)
  (declare (indent 0))
  `(nomis/scrolling/-with-maybe-maintain-line-no-in-window* (lambda () ,@body)
                                                            t))

;;;; Improve autoscrolling, v1

(defconst nomis/scrolling/-scroll-conservatively 101) ; never recenter

;; Emacs's autoscrolling is a bit shit, IMO.
;;
;; Simply doing
;;   `(setq scroll-conservatively nomis/scrolling/-scroll-conservatively)`
;; does what I want for scrolling up and down, but with that jumping to
;; a definition can put the top of the definition at the bottom of the window.
;;
;; This code sets that up only for commands that don't "jump".

(defconst nomis/scrolling/-commands-that-jump
  '(bookmark-jump
    compilation-next-error-function
    compile-goto-error
    find-file
    find-file-other-window
    find-function
    find-variable
    goto-line
    ido-exit-minibuffer
    ido-find-file
    ido-find-file-other-window
    imenu
    isearch-backward
    isearch-backward-regexp
    isearch-exit
    isearch-forward
    isearch-forward-regexp
    isearch-repeat-backward
    isearch-repeat-forward
    jump-to-register
    next-error
    magit-diff-visit-file
    magit-diff-visit-file-other-frame
    magit-diff-visit-file-other-window
    magit-diff-visit-worktree-file
    magit-diff-visit-worktree-file-other-frame
    magit-diff-visit-worktree-file-other-window
    nomis/clojure-lsp-and-cider/find-definition
    nomis/clojure-lsp-and-cider/find-definition-v2
    occur-mode-goto-occurrence
    pop-global-mark
    previous-error
    xref-find-apropos
    xref-find-definitions
    xref-find-references
    xref-go-back
    xref-go-forward
    xref-goto-xref
    ;;
    nomis/dirtree/display-file
    nomis/dirtree/display-file-and-goto-other-window
    nomis/dirtree/display-file-in-new-frame
    nomis/dirtree/goto-file
    nomis/dirtree/goto-file/no-create-window
    nomis/dirtree/goto-file/return-to-window
    nomis/dirtree/make-dirtree
    nomis/dirtree/show
    nomis/dirtree/next-line-and-display
    nomis/dirtree/previous-line-and-display
    nomis/dirtree/next-line-with-expansion-and-display
    nomis/dirtree/up-directory-and-display
    nomis/dirtree/next-sib-and-display
    nomis/dirtree/previous-sib-and-display
    nomis/dirtree/goto-root-and-display
    nomis/dirtree/scroll-up-and-display
    nomis/dirtree/scroll-down-and-display
    nomis/dirtree/history-step-back-and-display
    nomis/dirtree/history-step-forward-and-display)
  "Commands for which aggressive scrolling should NOT be applied.")

(defvar nomis/scrolling/-original-scroll-conservatively 0)

(add-hook 'emacs-startup-hook
          (lambda ()
            ;; Capture default value after all init files have loaded. Gap:
            ;; customizations via deferred loading (e.g. `with-eval-after-load`,
            ;; `use-package :defer`) that set `scroll-conservatively` on first
            ;; use of a package will not be captured here.
            (setq nomis/scrolling/-original-scroll-conservatively
                  (default-value 'scroll-conservatively))))

(defun nomis/scrolling/-set-aggressive-scrolling-for-command ()
  (setq scroll-conservatively
        (if (memq this-command nomis/scrolling/-commands-that-jump)
            nomis/scrolling/-original-scroll-conservatively
          nomis/scrolling/-scroll-conservatively))
  (nomis/scrolling/-debug "scroll-conservatively = %s for %s"
                          scroll-conservatively
                          this-command))

(add-hook 'pre-command-hook #'nomis/scrolling/-set-aggressive-scrolling-for-command)

;;; End

(provide 'nomis-scrolling)
