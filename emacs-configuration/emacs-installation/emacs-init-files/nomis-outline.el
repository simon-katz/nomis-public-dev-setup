;;; nomis-outline.el ---  -*- lexical-binding: t; -*-

;;; Code:

;;;; Requires

(require 'nomis-msg)
(require 'nomis-outline-wrappers)
(require 'nomis-tree-lineage-specs)
(require 'outline)

;;;; Ellipses

;; Copy-and-hack from
;; https://www.reddit.com/r/emacs/comments/e2u5n9/code_folding_with_outlineminormode/
(set-display-table-slot standard-display-table
                        'selective-display
                        (let ((face-offset (* (face-id 'shadow) (ash 1 22))))
                          (vconcat (mapcar (lambda (c) (+ face-offset c))
                                           " ▶▶▶"))))

;;;; outline-minor-mode

(add-hook 'prog-mode-hook 'outline-minor-mode)

(setopt outline-minor-mode-use-buttons 'in-margins)

;;;; outline-minor-faces

(use-package outline-minor-faces
  :after outline
  ;; We now call `outline-minor-faces-mode`, if we want it, in sidecar-locals.
  ;; :config (add-hook 'outline-minor-mode-hook
  ;;                   #'outline-minor-faces-mode)
  )

(defun -nomis/hack-outline-minor-faces ()
  ;; Previously we had:
  ;;
  ;;     (face-remap-add-relative 'outline-minor-0
  ;;                              :weight 'normal
  ;;                              :overline t
  ;;                              :background (face-background 'default))
  ;;
  ;; (`:overline t` means to use the foreground color,)
  ;;
  ;; That doesn't work. When another face (for me `hl-line`) specifies
  ;; `:underline`, the overline color is not the foreground color — instead it's
  ;; the same as the underline color.
  ;;
  ;; This appears to be an Emacs face merging quirk.
  ;;
  ;; Instead we change each of `outline-minor-faces` individually.
  (face-remap-add-relative 'outline-minor-0
                           :weight 'normal
                           :background (face-background 'default))
  (cl-loop for face across outline-minor-faces
           do (let* ((fg (face-foreground face nil 'default)))
                (face-remap-add-relative face :overline fg))))

;; If we decide that the extra vertical space for overlines is bad, we can do:
;; `(setq overline-margin 0)`.

(add-hook 'outline-minor-faces-mode-hook '-nomis/hack-outline-minor-faces)

;;;; Outline colors

(defun nomis/outline/colors/set-default ()
  (set-face-attribute 'outline-1 nil :inherit 'font-lock-function-name-face)
  (set-face-attribute 'outline-2 nil :inherit 'font-lock-variable-name-face)
  (set-face-attribute 'outline-3 nil :inherit 'font-lock-keyword-face)
  (set-face-attribute 'outline-4 nil :inherit 'font-lock-comment-face)
  (set-face-attribute 'outline-5 nil :inherit 'font-lock-type-face)
  (set-face-attribute 'outline-6 nil :inherit 'font-lock-constant-face)
  (set-face-attribute 'outline-7 nil :inherit 'font-lock-builtin-face)
  (set-face-attribute 'outline-8 nil :inherit 'font-lock-string-face))

(defun nomis/outline/colors/set-rainbow-8 ()
  ;; 8 colors in rainbow order, at least when using a dark theme.
  (set-face-attribute 'outline-1 nil :inherit 'font-lock-string-face)
  (set-face-attribute 'outline-2 nil :inherit 'font-lock-variable-name-face)
  (set-face-attribute 'outline-3 nil :inherit 'font-lock-type-face)
  (set-face-attribute 'outline-4 nil :inherit 'font-lock-constant-face)
  (set-face-attribute 'outline-5 nil :inherit 'font-lock-keyword-face)
  (set-face-attribute 'outline-6 nil :inherit 'font-lock-function-name-face)
  (set-face-attribute 'outline-7 nil :inherit 'font-lock-comment-face)
  (set-face-attribute 'outline-8 nil :inherit 'font-lock-builtin-face))

(defun nomis/outline/colors/set-rainbow-4 ()
  ;; 4 colors in rainbow order, at least when using a dark theme.
  ;;
  ;; "Of York Gave Battle"
  (set-face-attribute 'outline-1 nil :inherit 'font-lock-string-face)
  (set-face-attribute 'outline-2 nil :inherit 'font-lock-variable-name-face)
  (set-face-attribute 'outline-3 nil :inherit 'font-lock-constant-face)
  (set-face-attribute 'outline-4 nil :inherit 'font-lock-function-name-face)
  (set-face-attribute 'outline-5 nil :inherit 'outline-1)
  (set-face-attribute 'outline-6 nil :inherit 'outline-2)
  (set-face-attribute 'outline-7 nil :inherit 'outline-3)
  (set-face-attribute 'outline-8 nil :inherit 'outline-4))

;; (nomis/outline/colors/set-default)
;; (nomis/outline/colors/set-rainbow-8)
(nomis/outline/colors/set-rainbow-4)

;;;; Show entry when jumping from other places

(defun -nomis/outline/show-after-find/after-advice (&rest _)
  (when (or (derived-mode-p 'outline-mode)
            (bound-and-true-p outline-minor-mode))
    (nomis/tree/ls/show-after-find)))

(eval-after-load 'simple
  (progn
    (advice-add 'previous-error
                :after
                #'-nomis/outline/show-after-find/after-advice)
    (advice-add 'next-error
                :after
                #'-nomis/outline/show-after-find/after-advice)))

(eval-after-load 'compile
  (advice-add 'compilation-next-error-function
              :after
              #'-nomis/outline/show-after-find/after-advice))

(eval-after-load 'xref
  (progn
    (advice-add 'xref-find-definitions
                :after
                #'-nomis/outline/show-after-find/after-advice)
    (advice-add 'xref-goto-xref
                :after
                #'-nomis/outline/show-after-find/after-advice)))

(eval-after-load 'nomis-clojure-lsp
  (progn
    (advice-add 'nomis/clojure-lsp-and-cider/find-definition
                :after
                #'-nomis/outline/show-after-find/after-advice)
    (advice-add 'nomis/clojure-lsp-and-cider/find-definition-v2
                :after
                #'-nomis/outline/show-after-find/after-advice)))

(eval-after-load 'magit-diff
  (progn
    (advice-add 'magit-diff-visit-worktree-file
                :after
                #'-nomis/outline/show-after-find/after-advice)
    (advice-add 'magit-diff-visit-worktree-file-other-window
                :after
                #'-nomis/outline/show-after-find/after-advice)))

;;;; `outline-regexp`

;;;;; Emacs Lisp

(defun -nomis/set-emacs-lisp-outline ()
  ;; A hacked version of the `(setq-local outline-regexp ...)` in `lisp-mode`
  ;; that doesn't include top-level forms.
  (setq-local outline-regexp ";;;;*"))

(add-hook 'emacs-lisp-mode-hook '-nomis/set-emacs-lisp-outline)

;;;;; Clojure

(defun -nomis/set-clojure-outline ()
  ;; Note that we are changing the value of `outline-regexp` set in
  ;; `clojure-mode-variables`.
  ;;
  ;; Set `outline-regexp` to `;;;;` (and more semicolons) comments only, not
  ;; top-level forms.
  (setq-local outline-regexp ";;;;;*")
  (setq-local outline-level (lambda ()
                              (save-excursion
                                (looking-at outline-regexp)
                                (- (match-end 0)
                                   (match-beginning 0)
                                   3 ; 4 semicolons is level 1, so subtract 3
                                   )))))

(add-hook 'clojure-mode-hook '-nomis/set-clojure-outline)

;;;;; SCSS mode

(defun -nomis/set-scss-mode-outline ()
  (setq-local outline-regexp "/////*"))

(add-hook 'scss-mode-hook '-nomis/set-scss-mode-outline)

;;;; Fix `outline-mark-subtree` so that selection goes to next heading

(defun nomis/outline/mark-subtree/extend (&rest _)
  "If appropriate, extend region if mark is not on a heading."
  (when (use-region-p)
    (save-excursion
      (goto-char (mark))
      (while (and (not (outline-on-heading-p))
                  (not (eobp)))
        (forward-char 1))
      (set-mark (point)))))

(advice-add 'outline-mark-subtree
            :after
            #'nomis/outline/mark-subtree/extend
            '((name . -nomis/outline/mark-subtree/extend)))

;; (advice-remove 'outline-mark-subtree '-nomis/outline/mark-subtree/extend)

;;;; Support for restoring blank line convention when moving subtrees

(defun -nomis/outline/section-ends-with-blank? (pos)
  "Does the section at POS end with a blank line?"
  (save-excursion
    (goto-char pos)
    (outline-end-of-subtree)
    (if (eobp)
        ;; At eobp; two final \n chars = trailing blank line.
        (and (> (point) 2)
             (eq ?\n (char-before))
             (eq ?\n (char-before (1- (point)))))
      ;; At the \n before the next heading; preceding \n = blank line.
      (eq ?\n (char-before)))))

(defun -nomis/outline/blank-lines-follow-convention? (p1-before p2-before)
  "Return non-nil if the blank-line convention holds for a potential peer swap.
Specifically:
- Potential down-swap: current ends with blank; next peer (last) ends without.
- Potential up-swap: current (last) ends without blank; prev peer ends with."
  (when (nomis/outline/w/outline-mode?)
    (cond
     ((and p1-before (null p2-before))
      ;; Potential down-swap: current is second-to-last peer
      (and (-nomis/outline/section-ends-with-blank? (point))
           (not (-nomis/outline/section-ends-with-blank? p1-before))))
     ((null p1-before)
      ;; Potential up-swap: current is last peer
      (let* ((prev-peer
              (nomis/outline/w/prev-or-next-heading/pos 1 :backward :peer)))
        (and prev-peer
             (not (-nomis/outline/section-ends-with-blank? (point)))
             (-nomis/outline/section-ends-with-blank? prev-peer)))))))

(defun -nomis/outline/fix-blank-lines-for-last-heading (pos)
  (save-excursion
    (goto-char pos)
    (outline-end-of-subtree)
    (delete-char -1)
    (goto-char pos)
    (insert "\n")))

(defun -nomis/outline/maybe-fix-blank-lines (p1-before
                                             p2-before
                                             p1-after
                                             p2-after)
  (let* ((down-swap?
          (and p1-before (null p2-before) (null p1-after) (null p2-after)))
         (up-swap?
          (and (null p1-before) (null p2-before) p1-after (null p2-after))))
    (cond
     (down-swap?
      (-nomis/outline/fix-blank-lines-for-last-heading (point))
      (forward-char 1))
     (up-swap?
      (-nomis/outline/fix-blank-lines-for-last-heading p1-after)))))

(defvar *nomis/outline/restore-blank-lines/active* nil
  "Non-nil when `nomis/outline/with-restore-blank-lines` is active.
Used to prevent re-entrant blank-line fix attempts.")

(defmacro nomis/outline/with-restore-blank-lines (&rest body)
  (declare (indent 0))
  `(if *nomis/outline/restore-blank-lines/active*
       (progn ,@body)
     (let* ((*nomis/outline/restore-blank-lines/active* t)
            (p1-before
             (nomis/outline/w/prev-or-next-heading/pos 1 :forward :peer))
            (p2-before
             (nomis/outline/w/prev-or-next-heading/pos 2 :forward :peer))
            (blank-lines-follow-convention?
             (-nomis/outline/blank-lines-follow-convention? p1-before p2-before)))
       (prog1
           (progn ,@body)
         (when blank-lines-follow-convention?
           (let* ((p1-after
                   (nomis/outline/w/prev-or-next-heading/pos 1 :forward :peer))
                  (p2-after
                   (nomis/outline/w/prev-or-next-heading/pos 2 :forward :peer)))
             (-nomis/outline/maybe-fix-blank-lines p1-before
                                                   p2-before
                                                   p1-after
                                                   p2-after)))))))

;;;; Move subtrees

(defun nomis/outline/move-subtree-up/peer ()
  "Move subtree backward to previous peer position.
If there is no previous peer position, display a popup message."
  (interactive)
  (nomis/outline/w/back-to-heading)
  (let* ((sibling-pos
          (nomis/outline/w/prev-or-next-heading/pos 1 :backward :sibling))
         (peer-pos
          (nomis/outline/w/prev-or-next-heading/pos 1 :backward :peer)))
    (if peer-pos
        (if sibling-pos
            (nomis/outline/w/move-subtree-up)
          (nomis/outline/with-restore-blank-lines
            (nomis/outline/w/cut-subtree)
            (goto-char peer-pos)
            (nomis/tree/ls/show-after-nav)
            (nomis/outline/w/paste-subtree)
            (nomis/outline/w/move-subtree-down)))
      (nomis/outline/w/prev-or-next-heading/error-message 1 :backward :peer))))

(defun nomis/outline/move-subtree-down/peer ()
  "Move subtree forward to next peer position.
If there is no next peer position, display a popup message."
  (interactive)
  (nomis/outline/w/back-to-heading)
  (let* ((sibling-pos
          (nomis/outline/w/prev-or-next-heading/pos 1 :forward :sibling))
         (peer-pos
          (nomis/outline/w/prev-or-next-heading/pos 1 :forward :peer)))
    (if peer-pos
        (if sibling-pos
            (nomis/outline/w/move-subtree-down)
          (nomis/outline/with-restore-blank-lines
            (let* ((peer-marker (copy-marker peer-pos)))
              (nomis/outline/w/cut-subtree)
              (goto-char peer-marker)
              (set-marker peer-marker nil)
              (nomis/tree/ls/show-after-nav)
              (nomis/outline/w/paste-subtree))))
      (nomis/outline/w/prev-or-next-heading/error-message 1 :forward :peer))))

(defun -nomis/outline/move-subtree/restore-blank-lines/advice (orig-fn
                                                               &rest args)
  (nomis/outline/with-restore-blank-lines
    (apply orig-fn args)))

(advice-add 'outline-move-subtree-up
            :around
            #'-nomis/outline/move-subtree/restore-blank-lines/advice)

(advice-add 'outline-move-subtree-down
            :around
            #'-nomis/outline/move-subtree/restore-blank-lines/advice)

;; (advice-remove 'outline-move-subtree-up #'-nomis/outline/move-subtree/restore-blank-lines/advice)
;; (advice-remove 'outline-move-subtree-down #'-nomis/outline/move-subtree/restore-blank-lines/advice)

;;; End

(provide 'nomis-outline)
