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

;; /OFF/(defconst nomis/scrolling/-scroll-conservatively 101) ; never recenter
;; /OFF/
;; /OFF/;; Emacs's autoscrolling is a bit shit, IMO.
;; /OFF/;;
;; /OFF/;; Simply doing
;; /OFF/;;   `(setq scroll-conservatively nomis/scrolling/-scroll-conservatively)`
;; /OFF/;; does what I want for scrolling up and down, but with that jumping to
;; /OFF/;; a definition can put the top of the definition at the bottom of the window.
;; /OFF/;;
;; /OFF/;; This code sets that up only for commands that don't "jump".
;; /OFF/
;; /OFF/(defconst nomis/scrolling/-commands-that-jump
;; /OFF/  '(bookmark-jump
;; /OFF/    compilation-next-error-function
;; /OFF/    compile-goto-error
;; /OFF/    find-file
;; /OFF/    find-file-other-window
;; /OFF/    find-function
;; /OFF/    find-variable
;; /OFF/    goto-line
;; /OFF/    ido-exit-minibuffer
;; /OFF/    ido-find-file
;; /OFF/    ido-find-file-other-window
;; /OFF/    imenu
;; /OFF/    isearch-backward
;; /OFF/    isearch-backward-regexp
;; /OFF/    isearch-exit
;; /OFF/    isearch-forward
;; /OFF/    isearch-forward-regexp
;; /OFF/    isearch-repeat-backward
;; /OFF/    isearch-repeat-forward
;; /OFF/    jump-to-register
;; /OFF/    next-error
;; /OFF/    magit-diff-visit-file
;; /OFF/    magit-diff-visit-file-other-frame
;; /OFF/    magit-diff-visit-file-other-window
;; /OFF/    magit-diff-visit-worktree-file
;; /OFF/    magit-diff-visit-worktree-file-other-frame
;; /OFF/    magit-diff-visit-worktree-file-other-window
;; /OFF/    nomis/clojure-lsp-and-cider/find-definition
;; /OFF/    nomis/clojure-lsp-and-cider/find-definition-v2
;; /OFF/    occur-mode-goto-occurrence
;; /OFF/    pop-global-mark
;; /OFF/    previous-error
;; /OFF/    xref-find-apropos
;; /OFF/    xref-find-definitions
;; /OFF/    xref-find-references
;; /OFF/    xref-go-back
;; /OFF/    xref-go-forward
;; /OFF/    xref-goto-xref
;; /OFF/    ;;
;; /OFF/    nomis/dirtree/display-file
;; /OFF/    nomis/dirtree/display-file-and-goto-other-window
;; /OFF/    nomis/dirtree/display-file-in-new-frame
;; /OFF/    nomis/dirtree/goto-file
;; /OFF/    nomis/dirtree/goto-file/no-create-window
;; /OFF/    nomis/dirtree/goto-file/return-to-window
;; /OFF/    nomis/dirtree/make-dirtree
;; /OFF/    nomis/dirtree/show
;; /OFF/    nomis/dirtree/next-line-and-display
;; /OFF/    nomis/dirtree/previous-line-and-display
;; /OFF/    nomis/dirtree/next-line-with-expansion-and-display
;; /OFF/    nomis/dirtree/up-directory-and-display
;; /OFF/    nomis/dirtree/next-sib-and-display
;; /OFF/    nomis/dirtree/previous-sib-and-display
;; /OFF/    nomis/dirtree/goto-root-and-display
;; /OFF/    nomis/dirtree/scroll-up-and-display
;; /OFF/    nomis/dirtree/scroll-down-and-display
;; /OFF/    nomis/dirtree/history-step-back-and-display
;; /OFF/    nomis/dirtree/history-step-forward-and-display)
;; /OFF/  "Commands for which aggressive scrolling should NOT be applied.")
;; /OFF/
;; /OFF/(defvar nomis/scrolling/-original-scroll-conservatively 0)
;; /OFF/
;; /OFF/(add-hook 'emacs-startup-hook
;; /OFF/          (lambda ()
;; /OFF/            ;; Capture default value after all init files have loaded. Gap:
;; /OFF/            ;; customizations via deferred loading (e.g. `with-eval-after-load`,
;; /OFF/            ;; `use-package :defer`) that set `scroll-conservatively` on first
;; /OFF/            ;; use of a package will not be captured here.
;; /OFF/            (setq nomis/scrolling/-original-scroll-conservatively
;; /OFF/                  (default-value 'scroll-conservatively))))
;; /OFF/
;; /OFF/(defun nomis/scrolling/-set-aggressive-scrolling-for-command ()
;; /OFF/  (setq scroll-conservatively
;; /OFF/        (if (memq this-command nomis/scrolling/-commands-that-jump)
;; /OFF/            nomis/scrolling/-original-scroll-conservatively
;; /OFF/          nomis/scrolling/-scroll-conservatively))
;; /OFF/  (nomis/scrolling/-debug "scroll-conservatively = %s for %s"
;; /OFF/                          scroll-conservatively
;; /OFF/                          this-command))
;; /OFF/
;; /OFF/(add-hook 'pre-command-hook #'nomis/scrolling/-set-aggressive-scrolling-for-command)

;;;; Improve autoscrolling, v2

;;;;; Notes

;; `scroll-preserve-screen-position` is read inside `scroll-up-command` and
;; `scroll-down-command`, so we can use `:around` advice to set it.

;; `scroll-conservatively` is read in the post-command redraw phase.
;; The simplest way to set a temporary value is to use `:after` advice that sets
;; up a binding and calls `redisplay` explicitly.

;;;;; Maintain screen position on Page Up / Page Down

(advice-add 'scroll-up-command
            :around
            (lambda (orig-fun &rest args)
              (let* ((scroll-preserve-screen-position t))
                (apply orig-fun args)))
            '((name . nomis/scroll-preserve-screen-position)))

(advice-add 'scroll-down-command
            :around
            (lambda (orig-fun &rest args)
              (let* ((scroll-preserve-screen-position t))
                (apply orig-fun args)))
            '((name . nomis/scroll-preserve-screen-position)))

;; (progn
;;   (advice-remove 'scroll-up-command 'nomis/scroll-preserve-screen-position)
;;   (advice-remove 'scroll-down-command 'nomis/scroll-preserve-screen-position))

;;;;; nomis/scrolling/conservative-mode

;;;;;; The mode itself

(defvar nomis/scrolling/conservative-mode-map
  (make-sparse-keymap))

(define-minor-mode nomis/scrolling/conservative-mode
  "Minor mode overriding movement commands with conservative-scrolling versions."
  :global t
  :group 'nomis/scrolling
  :keymap nomis/scrolling/conservative-mode-map)

(add-to-list 'emulation-mode-map-alists
             `((nomis/scrolling/conservative-mode
                . ,nomis/scrolling/conservative-mode-map)))

(define-key global-map (kbd "C-M-ç") ; C-Option-M-c
            #'nomis/scrolling/conservative-mode)

;;;;;; nomis/scrolling/define-conservative-scroller

(cl-defmacro nomis/scrolling/define-conservative-scroller (name key base-command)
  (declare (indent 1))
  `(progn
     (defun ,name (&optional arg)
       ,(format "Call `%s' with conservative scrolling." base-command)
       (interactive "^p")
       (let* ((scroll-conservatively 101))
         (,base-command arg)
         (redisplay) ; scroll while let-binding is still alive
         ))
     (define-key nomis/scrolling/conservative-mode-map (kbd ,key)
                 ',name)))

;;;;;; Scroll conservatively by paragraph

(nomis/scrolling/define-conservative-scroller
    nomis/scrolling/backward-paragraph-conservatively
  "C-<up>" backward-paragraph)

(nomis/scrolling/define-conservative-scroller
    nomis/scrolling/forward-paragraph-conservatively
  "C-<down>" forward-paragraph)

;;;;;; Scroll conservatively by sexp

(nomis/scrolling/define-conservative-scroller
    nomis/scrolling/backward-sexp-conservatively
  "C-M-b" backward-sexp)

(nomis/scrolling/define-conservative-scroller
    nomis/scrolling/forward-sexp-conservatively
  "C-M-f" forward-sexp)

;;;;;; Scroll conservatively by defun

(nomis/scrolling/define-conservative-scroller
    nomis/scrolling/beginning-of-defun-conservatively
  "C-M-a" beginning-of-defun)

(nomis/scrolling/define-conservative-scroller
    nomis/scrolling/end-of-defun-conservatively
  "C-M-e" end-of-defun)

;;;;;; Scroll conservatively by list

(nomis/scrolling/define-conservative-scroller
    nomis/scrolling/forward-list-conservatively
  "C-M-n" forward-list)

(nomis/scrolling/define-conservative-scroller
    nomis/scrolling/backward-list-conservatively
  "C-M-p" backward-list)

;;;;;; Scroll conservatively for `down-list` and `backward-up-list`

(nomis/scrolling/define-conservative-scroller
    nomis/scrolling/down-list-conservatively
  "C-M-d" down-list)

(nomis/scrolling/define-conservative-scroller
    nomis/scrolling/backward-up-list-conservatively
  "C-M-u" backward-up-list)

;;;;;; Scroll conservatively by sentence

(nomis/scrolling/define-conservative-scroller
    nomis/scrolling/backward-sentence-conservatively
  "M-a" backward-sentence)

(nomis/scrolling/define-conservative-scroller
    nomis/scrolling/forward-sentence-conservatively
  "M-e" forward-sentence)

;;; End

(provide 'nomis-scrolling)
