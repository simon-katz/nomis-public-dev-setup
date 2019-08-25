;;;; Init stuff -- Org Mode

(progn) ; this-stops-hs-hide-all-from-hiding-the-next-comment

;;;; ___________________________________________________________________________
;;;; ____ * Require things

(require 'org)
(require 'org-bullets)
(require 'cl)
(require 'dash)
(require 'nomis-repeated-commands)

;;;; ___________________________________________________________________________
;;;; ____ * General

(setq org-directory "~/org")

(setq org-log-done nil)

(setq org-return-follows-link t)

(setq org-startup-indented t)

(progn
  ;; Use current window when clicking links.
  (setq org-link-frame-setup
        (acons 'file
               'find-file
               org-link-frame-setup)))

;;;; ___________________________________________________________________________
;;;; ____ * Towards a nicer API

;;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;;;; ____ ** Last command

(defun nomis/org/last-command ()
  (or (bound-and-true-p *nomis/smex/last-command*)
      last-command))

;;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;;;; ____ ** Misc

(defun nomis/org/map-roots (fun &optional match scope &rest skip)
  (apply #'org-map-entries
         (lambda ()
           (let* ((level (nomis/org/current-level)))
             (when (= level 1)
               (funcall fun))))
         match
         scope
         skip))

(defun nomis/org/current-level ()
  (nth 1 (org-heading-components)))

(defun nomis/org/levels/n-below-point ()
  (let* ((current-level
          (nomis/org/current-level))
         (max-level-beneath
          (let* ((sofar 0))
            (org-map-entries (lambda (&rest _)
                               (setq sofar (max (nomis/org/current-level)
                                                sofar)))
                             t
                             'tree)
            sofar)))
    (- max-level-beneath
       current-level)))

(defun nomis/org/levels/max-below-root ()
  (save-excursion
    (nomis/org/goto-root)
    (nomis/org/levels/n-below-point)))

(defun nomis/org/levels/max-in-buffer ()
  (let* ((sofar 0))
    (org-map-entries (lambda (&rest _)
                       (setq sofar (max (nomis/org/current-level)
                                        sofar)))
                     t
                     'file)
    sofar))

(defun nomis/org/report-org-info ()
  (interactive)
  (message "Current level = %s%s"
           (nomis/org/current-level)
           (if (not (fboundp 'nomis/point-etc-string))
               ""
             (concat "    "
                     (nomis/point-etc-string)))))

;;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;;;; ____ ** show-children

(defun nomis/org/show-children (n)
  "Expand current headline to n levels.

Details:

If N is not negative, expand to show N levels. Any headlines at level N
will be collapsed.

If N is negative, expand to show (abs N) levels, but do not hide anything
that is already being displayed."
  (interactive "^p")
  (let* ((collapse? (>= n 0))
         (n (abs n)))
    (when collapse?
      (-nomis/org/collapse))
    (outline-show-children n)))

;;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;;;; ____ ** show-children/incremental

(nomis/define-repeated-command-stuff
    -nomis/org/show-children/incremental
  nomis/org/show-children
  -nomis/org/show-children/incremental/with-stuff/incremental
  -nomis/org/show-children/incremental/with-stuff/set
  -nomis/org/show-children/incremental/previous-values
  (let* ((v (+ %previous-value% %in-value%))
         (maximum (nomis/org/levels/n-below-point)))
    (when (or (< v 0)
              (> v maximum))
      (nomis/grab-user-attention/low))
    (min (max 0 v)
         maximum))
  (lambda (v)
    (format "[%s of %s]"
            v
            (nomis/org/levels/n-below-point) ; TODO Maybe too expensive
            )))

(defun nomis/org/show-children/set-0 ()
  (interactive)
  (-nomis/org/show-children/incremental/with-stuff/set 0))

(defun nomis/org/show-children/fully-expand ()
  (interactive)
  (-nomis/org/show-children/incremental/with-stuff/set
   (nomis/org/levels/n-below-point)))

(defun nomis/org/show-children/incremental/less ()
  (interactive)
  (-nomis/org/show-children/incremental/with-stuff/incremental 0 -1))

(defun nomis/org/show-children/incremental/more ()
  (interactive)
  (-nomis/org/show-children/incremental/with-stuff/incremental 1 1))

;;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;;;; ____ ** show-children-from-root

(defun nomis/org/show-children-from-root (n)
  (interactive "^p")
  "Call `nomis/org/show-children` on the current root headline, with N as
the parameter.
When done, call `org-reveal` so that the current point is shown.
But see ++about-uses-of-org-reveal++"
  (save-excursion
    (nomis/org/goto-root)
    (nomis/org/show-children n))
  (org-reveal) ; see ++about-uses-of-org-reveal++
  )

;;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;;;; ____ ** show-children-from-root/incremental

(nomis/define-repeated-command-stuff
    -nomis/org/show-children-from-root/incremental
  nomis/org/show-children-from-root
  -nomis/org/show-children-from-root/incremental/with-stuff/incremental
  -nomis/org/show-children-from-root/incremental/with-stuff/set
  -nomis/org/show-children-from-root/incremental/previous-values
  (let* ((v (+ %previous-value% %in-value%))
         (maximum (nomis/org/levels/max-below-root)))
    (when (or (< v 0)
              (> v maximum))
      (nomis/grab-user-attention/low))
    (min (max 0 v)
         maximum))
  (lambda (v)
    (format "[%s of %s] from root"
            v
            (nomis/org/levels/max-below-root) ; TODO Maybe too expensive
            )))

(defun nomis/org/show-children-from-root/set-0 ()
  (interactive)
  (save-excursion
    (nomis/org/goto-root) ; share the same point->level mapping
    (-nomis/org/show-children-from-root/incremental/with-stuff/set 0)))

(defun nomis/org/show-children-from-root/fully-expand ()
  (interactive)
  (save-excursion
    (nomis/org/goto-root) ; share the same point->level mapping
    (-nomis/org/show-children-from-root/incremental/with-stuff/set
     (nomis/org/levels/max-below-root))))

(defun nomis/org/show-children-from-root/incremental/less ()
  (interactive)
  (save-excursion
    (nomis/org/goto-root) ; share the same point->level mapping
    (-nomis/org/show-children-from-root/incremental/with-stuff/incremental 0 -1)))

(defun nomis/org/show-children-from-root/incremental/more ()
  (interactive)
  (save-excursion
    (nomis/org/goto-root) ; share the same point->level mapping
    (-nomis/org/show-children-from-root/incremental/with-stuff/incremental 1 1)))

;;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;;;; ____ ** show-children-from-all-roots

(defun nomis/org/show-children-from-all-roots (n)
  "Call `nomis/org/show-children` on all root headlines, with N as
the parameter.
When done, call `org-reveal` so that the current point is shown.
But see ++about-uses-of-org-reveal++"
  (interactive "^p")
  (save-excursion
    (nomis/org/map-roots (lambda () (nomis/org/show-children n))
                         t
                         'file))
  (org-reveal) ; see ++about-uses-of-org-reveal++
  )

;;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;;;; ____ ** show-children-from-all-roots/incremental

(nomis/define-repeated-command-stuff
    -nomis/org/show-children-from-all-roots/incremental
  nomis/org/show-children-from-all-roots
  -nomis/org/show-children-from-all-roots/incremental/with-stuff/incremental
  -nomis/org/show-children-from-all-roots/incremental/with-stuff/set
  -nomis/org/show-children-from-all-roots/incremental/previous-values
  (let* ((v (+ %previous-value% %in-value%))
         (maximum (nomis/org/levels/max-in-buffer)))
    (when (or (< v 0)
              (> v maximum))
      (nomis/grab-user-attention/low))
    (min (max 0 v)
         maximum))
  (lambda (v)
    (format "[%s of %s] from all roots"
            v
            (nomis/org/levels/max-in-buffer) ; TODO Maybe too expensive
            )))

(defun nomis/org/show-children-from-all-roots/set-0 ()
  (interactive)
  (save-excursion
    (goto-char 1) ; share the same point->level mapping
    (-nomis/org/show-children-from-all-roots/incremental/with-stuff/set 0)))

(defun nomis/org/show-children-from-all-roots/fully-expand ()
  (interactive)
  (save-excursion
    (goto-char 1) ; share the same point->level mapping
    (-nomis/org/show-children-from-all-roots/incremental/with-stuff/set
     (nomis/org/levels/max-in-buffer))))

(defun nomis/org/show-children-from-all-roots/incremental/less ()
  (interactive)
  (save-excursion
    (goto-char 1) ; share the same point->level mapping
    (-nomis/org/show-children-from-all-roots/incremental/with-stuff/incremental
     0
     -1)))

(defun nomis/org/show-children-from-all-roots/incremental/more ()
  (interactive)
  (save-excursion
    (goto-char 1) ; share the same point->level mapping
    (-nomis/org/show-children-from-all-roots/incremental/with-stuff/incremental
     1
     1)))

;;;; ___________________________________________________________________________
;;;; ____ * Hiding and showing -- cycling

(defun -nomis/org-show-only (detail)
  (case 1
    (1 (org-overview))
    (2 (save-excursion
         (nomis/org/goto-root)
         (-nomis/org/collapse))))
  (org-show-set-visibility detail))

(defvar -nomis/org-show-only/cycle/visibility-spans
  ;;  See `org-show-context-detail`.
  '(minimal
    local
    ancestors
    lineage
    tree
    canonical))

(defvar -nomis/org-show-only/cycle/previous-place nil)
(defvar -nomis/org-show-only/cycle/previous-action-index -1)

(defun -nomis/org-show-only/cycle/next-position (n)
  ;; TODO Use the new approach.
  (let* ((current-place (list (current-buffer)
                              (point)))
         (previous-place -nomis/org-show-only/cycle/previous-place))
    (setq -nomis/org-show-only/cycle/previous-place
          current-place)
    (let* ((prev-action-index -nomis/org-show-only/cycle/previous-action-index)
           (action-index (if (not (equal current-place previous-place))
                             (position (if (< n 0) 'ancestors 'tree)
                                       -nomis/org-show-only/cycle/visibility-spans)
                           (+ n prev-action-index)))
           (ok? (<= 0
                    action-index
                    (1- (length -nomis/org-show-only/cycle/visibility-spans)))))
      (if ok?
          (progn
            (setq -nomis/org-show-only/cycle/previous-action-index
                  action-index)
            action-index)
        nil))))

(defun nomis/org-show-only/cycle/impl (n)
  (let* ((pos (-nomis/org-show-only/cycle/next-position n)))
    (if (null pos)
        (progn
          (if (< n 0)
              (message "Already at min span")
            (message "Already at max span"))
          (nomis/grab-user-attention/low))
      (let* ((visibility-span (nth pos
                                   -nomis/org-show-only/cycle/visibility-spans)))
        (message "Setting visibility-span = %s"
                 visibility-span)
        (-nomis/org-show-only visibility-span)))))

(defun nomis/org-show-only/cycle/more ()
  (interactive)
  (nomis/org-show-only/cycle/impl 1))

(defun nomis/org-show-only/cycle/less ()
  (interactive)
  (nomis/org-show-only/cycle/impl -1))

;;;; ___________________________________________________________________________
;;;; ____ * Priorities

(setq org-highest-priority ?1)
(setq org-lowest-priority  ?9)
(setq org-default-priority ?2)

;;;; ___________________________________________________________________________
;;;; ____ * Org mode hook function

(defun nomis/org-mode ()
  ;; Layout
  (linum-mode 0) ; see "Linum-mode + org-indent-mode gives strange graphical refresh bugs" at http://orgmode.org/worg/org-issues.html
  ;; (setq org-indent-fix-section-after-idle-time nil)
  ;; (setq org-indent-indentation-per-level 3) ; the default of 2 is too small; 4 screws up auto indentation in large files
  ;; (setq org-indent-max 60)
  ;; (setq org-indent-max-levels 80)

  ;; Copying and pasting
  (setq org-yank-adjusted-subtrees t)

  ;; Exporting
  ;; (setq org-export-headline-levels 3)
  ;; (setq org-export-initial-scope 'subtree) ; you can set this when running `org-export-dispatch`

  ;; Scrolling
  (setq org-cycle-hook
        (remq 'org-optimize-window-after-visibility-change
              org-cycle-hook))

  ;; Misc
  (visual-line-mode 1) ; Try this for a while and see whether you like it
  )

(add-hook 'org-mode-hook 'nomis/org-mode)

;;;; ___________________________________________________________________________
;;;; ____ * Dependencies

(setq org-enforce-todo-dependencies t)
;; (setq org-agenda-dim-blocked-tasks 'invisible) ; actually the default dimmimg is nice -- you can see more info

;;;; ___________________________________________________________________________
;;;; ____ * Capture

(setq org-default-notes-file
      (concat org-directory
              "/___captured-notes-from-"
              (substring (symbol-name nomis/system-name)
                         1)
              ".org"))

;;;; ___________________________________________________________________________
;;;; ____ * Navigation

(defun nomis/org/goto-root ()
  (interactive)
  (while (ignore-errors (outline-up-heading 1))))

(defun nomis/org-show-point ()
  (interactive)
  (case 1
    (1
     (when (get-char-property (point) 'invisible)
       ;; Make point visible and leave subtree collapsed
       (dotimes (_ 3) (org-cycle))))
    (2
     ;; This makes lots of stuff visible, but seems to be the "official" way.
     ;; Leave this here as a point of interest.
     (let ((org-catch-invisible-edits 'show))
       (org-check-before-invisible-edit 'insert)))))

(defun nomis/org-previous-heading ()
  (interactive)
  (outline-previous-heading)
  (nomis/org-show-point))

(defun nomis/org-next-heading ()
  (interactive)
  (outline-next-heading)
  (nomis/org-show-point))

(defun nomis/-org-heading-same-level-with-extras/helper (direction)
  (let ((start-position-fun (case direction
                              (:forward 'org-end-of-line)
                              (:backward 'org-beginning-of-line)))
        (re-search-function (case direction
                              (:forward 're-search-forward)
                              (:backward 're-search-backward)))
        (post-search-adjust-function (case direction
                                       (:forward 'org-beginning-of-line)
                                       (:backward #'(lambda ())))))
    (let* ((text-to-look-for (save-excursion
                               (org-beginning-of-line)
                               (concat (thing-at-point 'symbol)
                                       " "))))
      (funcall start-position-fun)
      (let ((found-p (condition-case nil
                         (progn
                           (funcall re-search-function
                                    (concat "^" (regexp-quote text-to-look-for))
                                    nil
                                    nil ;t
                                    )
                           t)
                       (error nil))))
        (if found-p
            (progn
              (nomis/org-show-point)
              (funcall post-search-adjust-function))
          (progn
            (org-beginning-of-line)
            (message (concat "No more headings at this level"
                             (when (eql direction :forward)
                               " (but there's a bug so maybe there are...)")))
            (beep)))))))

(defun nomis/org-forward-heading-same-level-with-extras ()
  "A replacement for `org-forward-heading-same-level`.
Move forward one subheading at same level as this one.
Works when the target is invisible (and makes it visible).
If this is the first subheading within its parent, move to the first
subheading at this level in the next parent."
  (interactive)
  (nomis/-org-heading-same-level-with-extras/helper :forward))

(defun nomis/org-backward-heading-same-level-with-extras ()
  "A replacement for `org-backward-heading-same-level`.
Move backward one subheading at same level as this one.
Works when the target is invisible (and makes it visible).
If this is the first subheading within its parent, move to the last
subheading at this level in the previous parent."
  (interactive)
  (nomis/-org-heading-same-level-with-extras/helper :backward))

;;;; ___________________________________________________________________________
;;;; ____ * Stepping

(defun -nomis/org/collapse ()
  (nomis/org-show-point)
  (case 2
    (1
     ;; This hides too much stuff.
     (org-overview)
     (org-show-set-visibility 'canonical))
    (2
     ;; This hides just the subtree under the headline at point.
     ;; Idea from http://christiantietze.de/posts/2019/06/org-fold-heading/.
     ;; But what does `org-flag-subtree` do, is it part of the org public API,
     ;; and why can't I find any useful info by googling?
     (org-flag-subtree t))))

(defun -nomis/org/expand ()
  (nomis/org-show-point)
  (-nomis/org/collapse) ; so that we can expand in a predicable way
  (case 3
    ;; I tried various approaches until I found one that seems to work.
    (1 (outline-show-children 99))
    (2 (dotimes (_ 5)
         ;; The 5 should work no matter how many levels there are below
         ;; this one. It does if you hit TAB five times.
         (org-cycle)))
    (3 (org-map-tree #'org-cycle) ; see also `org-map-tree` if you copy this
       )))

(defvar -nomis/org/step/previous-direction nil)
(defvar -nomis/org/step/previous-state nil)

(defvar -nomis/org/step/functions '())

(defun -nomis/org/step/impl (n jumping-parent-allowed?)
  (let* ((direction (if (< n 0) :backward :forward)))
    (cl-flet* ((previous-command-was-a-nomis-org-step?
                ()
                (member (nomis/org/last-command) -nomis/org/step/functions))
               (collapse-already-attempted?
                ()
                (member -nomis/org/step/previous-state
                        '(:cannot-move-and-collapsed
                          :cannot-move-and-issued-error)))
               (change-of-direction-when-collapsed?
                ()
                (and (not (eql direction -nomis/org/step/previous-direction))
                     (collapse-already-attempted?)))
               (record-new-state
                (x)
                (setq -nomis/org/step/previous-direction direction)
                (setq -nomis/org/step/previous-state x))
               (expand
                ()
                (-nomis/org/expand)
                (record-new-state :tried-to-expand))
               (collapse
                ()
                (-nomis/org/collapse)
                (record-new-state :cannot-move-and-collapsed))
               (tried-to-go-to-far
                ()
                (record-new-state :cannot-move-and-issued-error)
                (nomis/grab-user-attention/low)
                (error (if (< n 0)
                           "No previous heading at this level"
                         "No next heading at this level"))))
      (org-back-to-heading t)
      (cond ((not (previous-command-was-a-nomis-org-step?))
             (expand))
            ((change-of-direction-when-collapsed?)
             (expand))
            (t
             (-nomis/org/collapse)
             (let* ((starting-point (point)))
               (if jumping-parent-allowed?
                   (nomis/-org-heading-same-level-with-extras/helper
                    (case n
                      (1 :forward)
                      (-1 :backward)))
                 (org-forward-heading-same-level n t))
               (let* ((moved? (not (= (point) starting-point))))
                 (cond (moved?
                        ;; We moved. Expand the newly-arrived at heading.
                        (expand))
                       ((collapse-already-attempted?)
                        ;; We didn't move, and we've already tried to collapse.
                        (tried-to-go-to-far))
                       (t
                        ;; We didn't move, and we haven't yet tried to collapse.
                        (collapse))))))))))

(cl-defmacro define-nomis-org-step-command (name arglist &body body)
  (declare (indent 2))
  `(progn
     (pushnew ',name -nomis/org/step/functions)
     (defun ,name ,arglist ,@body)))

(define-nomis-org-step-command nomis/org/step-forward ()
  (interactive)
  (-nomis/org/step/impl 1 nil))

(define-nomis-org-step-command nomis/org/step-backward ()
  (interactive)
  (-nomis/org/step/impl -1 nil))

(define-nomis-org-step-command nomis/org/step-forward/jumping-parent-allowed ()
  (interactive)
  (-nomis/org/step/impl 1 t))

(define-nomis-org-step-command nomis/org/step-backward/jumping-parent-allowed ()
  (interactive)
  (-nomis/org/step/impl -1 t))

;;;; ___________________________________________________________________________
;;;; ____ * Refiling

;;;; - I haven't quite got this nice (or maybe I did).
;;;; - BUT:
;;;;   After a refile, undo only undoes what happened in one buffer, even
;;;;   though two buffers have been modified. That's crappy.

;;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;;;; ____ Play #1

;; (progn org-use-fast-todo-selection)

;; (setf org-refile-targets '((nil :maxlevel . 9)
;;                            (org-agenda-files :maxlevel . 9)))
;; (setf org-refile-use-outline-path t)
;; (setf org-outline-path-complete-in-steps t)
;; (setq org-refile-allow-creating-parent-nodes 'confirm)
;; (setq org-completion-use-ido nil)

;;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;;;; ____ Play #2 -- 2017-08-10

;; (defun nomis/org-refile-values ()
;;   (list org-refile-targets
;;         org-refile-use-outline-path
;;         org-outline-path-complete-in-steps
;;         org-refile-allow-creating-parent-nodes
;;         org-completion-use-ido))

;; (defun nomis/org-refile-reset ()
;;   (setq org-refile-targets nil
;;         org-refile-use-outline-path nil
;;         org-outline-path-complete-in-steps t
;;         org-refile-allow-creating-parent-nodes nil
;;         org-completion-use-ido nil))

;; (defun nomis/org-refile-setup ()
;;   (setq org-refile-targets '((nil :maxlevel . 9)
;;                              (org-agenda-files :maxlevel . 9))
;;         org-refile-use-outline-path nil ; 'file
;;         org-outline-path-complete-in-steps nil
;;         org-refile-allow-creating-parent-nodes nil
;;         org-completion-use-ido t))

;;;; ___________________________________________________________________________
;;;; ____ * Agendas

(require 'org-agenda)

(progn
  (defun nomis/org-reset-org-agenda-files ()
    (interactive)
    (setq org-agenda-files
          (progn
            (load-library "find-lisp")
            (find-lisp-find-files org-directory
                                  "\\\.org$"))))
  (defadvice org-todo-list (before reset-agenda-files
                                   activate compile)
    (nomis/org-reset-org-agenda-files)))

(progn
  (defun nomis/org-finalize-agenda-hook ()
    (hl-line-mode)
    ;; From http://orgmode.org/worg/org-faq.html
    ;;   How can I stop the mouse cursor from highlighting lines
    ;;   in the agenda?
    ;;     You can add the following to your .emacs:
    (remove-text-properties
     (point-min) (point-max) '(mouse-face t)))
  (add-hook 'org-finalize-agenda-hook
            'nomis/org-finalize-agenda-hook))

;;;; ___________________________________________________________________________
;;;; ____ * Fontify code in code blocks

(setq org-src-fontify-natively t)

;;;; ___________________________________________________________________________
;;;; ____ * orgstruct

;;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;;;; ____ orgstruct-mode general

;;;; To use, enable orgstruct-mode or orgstruct++-mode

(setq orgstruct-heading-prefix-regexp ";+ *\\(?:_+ \\)?")

(add-hook 'prog-mode-hook 'orgstruct-mode)

;;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;;;; ____ orgstruct-mode display

(defvar nomis/orgstruct-display-table nil
  ;; Approach copied from `org-display-table` stuff.
  "The display table for orgstruct-mode when `org-ellipsis' is non-nil.")

(defun nomis/orgstruct-set-up-display-table ()
  (if (not (member org-version
                   '("9.1.9")))
      (progn
        (nomis/grab-user-attention/low)
        (message "•••• You need to check `nomis/orgstruct-set-up-display-table` for this version of Org mode."))
    (when (and (stringp org-ellipsis) (not (equal "" org-ellipsis)))
      (unless nomis/orgstruct-display-table
        (setq nomis/orgstruct-display-table (make-display-table)))
      (set-display-table-slot
       nomis/orgstruct-display-table 4
       (vconcat (mapcar (lambda (c) (make-glyph-code c 'org-ellipsis))
                        org-ellipsis)))
      (setq buffer-display-table nomis/orgstruct-display-table))))

(add-hook 'orgstruct-mode-hook 'nomis/orgstruct-set-up-display-table)


;;;; ___________________________________________________________________________
;;;; ____ * Display -- misc

(setq org-ellipsis " ▶")

(setq org-bullets-bullet-list '("⦿"
                                "■"
                                "●"
                                "✖"
                                "♣"
                                "◑"
                                "◒"
                                "◐"
                                "◓"
                                "◮"
                                "◭"
                                "➀" "➁" "➂" "➃" "➄" "➅" "➆" "➇" "➈" "➉"
                                ;; "➊" "➋" "➌" "➍" "➎" "➏" "➐" "➑" "➒" "➓"
                                ;; "█" "▊" "▌" "▎"
                                ))

(add-hook 'org-mode-hook 'org-bullets-mode)

;;;; ___________________________________________________________________________
;;;; ____ * Display -- links

(defun nomis/org-show-link-destination ()
  ;; Copied with changes from
  ;; https://stackoverflow.com/questions/30312638/is-there-a-package-or-setting-to-show-an-org-mode-link-under-cursor-destinatio
  (when (memq major-mode
              '(org-mode
                org-agenda-mode))
    (ignore-errors ; sometimes this breaks, (?) and stops future ones running (?) (but maybe that was before checking the major-mode)
      (let ((object (org-element-context)))
        (when (eq (car object) 'link)
          (message "%s"
                   (org-element-property :raw-link object)))))))

(add-hook 'post-command-hook 'nomis/org-show-link-destination)


;;;; ___________________________________________________________________________
;;;; ____ * Export1

(defun get-string-from-file (path)
  "Return filePath's file content."
  (with-temp-buffer
    (insert-file-contents path)
    (buffer-string)))

(defconst nomis/org-export-apply-hacks-max-read-attempts 20)
(defconst nomis/org-export-apply-hacks-sleep-ms 100)

(cl-defun nomis/org-export-read-hacked-file
    (output-path &optional (n-attempts 1))
  (cl-flet ((sleep-a-bit
             ()
             (sleep-for 0 nomis/org-export-apply-hacks-sleep-ms)))
    (cond ((file-exists-p output-path)
           (sleep-a-bit) ; in case file exists but writing hasn't finished
           (get-string-from-file output-path))
          ((<= n-attempts nomis/org-export-apply-hacks-max-read-attempts)
           (sleep-a-bit)
           (nomis/org-export-read-hacked-file output-path (1+ n-attempts)))
          (t
           (beep)
           (error "FAILED: Tried %s times to read %s"
                  nomis/org-export-apply-hacks-max-read-attempts
                  output-path)))))

(cl-defun nomis/org-export-apply-hacks (&optional s)
  (let ((input-path (make-temp-file "__nomis-org-export--input-"))
        (output-path (make-temp-file "__nomis-org-export--output-")))
    (delete-file output-path) ; later we will wait for it to be created
    (write-region s nil input-path)
    (unwind-protect
        (progn
          (nomis/run-clojure-no-insert
           (format "(do (require '[nomis-blog.layer-2-domain.content.source.org-mode-source.pre-parse-transforms :as ppt])
                         (ppt/org-export-apply-hacks-to-file :latex
                                                              \"%s\"
                                                              \"%s\"))"
                   input-path
                   output-path))
          (nomis/org-export-read-hacked-file output-path))
      (ignore-errors (delete-file input-path))
      (ignore-errors (delete-file output-path)))))

(defun nomis/org-export ()
  (interactive)
  (let* ((s (buffer-string))
         (old-buffer (current-buffer))
         (name (-> old-buffer
                   buffer-file-name
                   file-name-nondirectory))
         (temp-name (concat "__zzzz--temp--nomis-org-export--" name))
         (new-buffer (generate-new-buffer temp-name)))
    (progn ; I did have `unwind-protect`, but that meant I didn't see errors
      (with-current-buffer new-buffer
        (let ((new-s (nomis/org-export-apply-hacks s)))
          (insert new-s))
        (write-file temp-name)
        (org-export-dispatch) ; user must select "latex", then "pdf"
        )
      (kill-buffer new-buffer)
      (delete-file temp-name)
      (let ((temp-name-sans-extension (file-name-sans-extension temp-name))
            (name-sans-extension (file-name-sans-extension name)))
        (rename-file (concat temp-name-sans-extension ".tex")
                     (concat name-sans-extension ".tex")
                     t)
        (rename-file (concat temp-name-sans-extension ".pdf")
                     (concat name-sans-extension ".pdf")
                     t)))))

;;;; Publishing

;;;; This doesn't work particularly well. Do it in a terminal window instead.

;; (defun nomis/ordinary-insertion-filter (proc string)
;;   ;; Copied, with changes, from
;;   ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Filter-Functions.html#Filter-Functions
;;   (when (buffer-live-p (process-buffer proc))
;;     (with-current-buffer (process-buffer proc)
;;       (let ((moving (case 2
;;                       (1 (= (point) (process-mark proc)))
;;                       (2 t))))
;;         (save-excursion
;;           ;; Insert the text, advancing the process marker.
;;           (goto-char (process-mark proc))
;;           (insert (s-replace "" "\n" string))
;;           (set-marker (process-mark proc) (point)))
;;         (if moving (goto-char (process-mark proc)))))))

;; (defun nomis/org-publish-filter-function (proc string)
;;   (nomis/ordinary-insertion-filter proc string))

;; (defun nomis/org-publish ()
;;   (interactive)
;;   (let (;; (password (read-passwd "Enter password: "))
;;         (output-buffer (get-buffer-create "nomis-org-publish")))
;;     (display-buffer output-buffer)
;;     (with-current-buffer output-buffer
;;       (goto-char (point-max))
;;       (insert "\n____________________________________\n"))
;;     (case 2
;;       (1 (call-process "/Users/simonkatz/development-100/repositories/nomis/nomis-blog/_scripts/publish.sh"
;;                        nil
;;                        output-buffer
;;                        t
;;                        ;; (concat "\"" password "\"")
;;                        ))
;;       (2 (make-process :name "nomis-org-publish"
;;                        :buffer output-buffer
;;                        :filter 'nomis/org-publish-filter-function
;;                        :command (list "/Users/simonkatz/development-100/repositories/nomis/nomis-blog/_scripts/publish.sh"
;;                                       ;; (concat "\"" password "\"")
;;                                       )
;;                        ;; :stderr output-buffer
;;                        )))))

;;;; ___________________________________________________________________________
;;;; * End

(provide 'nomis-org)
