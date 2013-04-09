;;;; Init stuff -- Org Mode.

;;;; ___________________________________________________________________________
;;;; The following lines are always needed. Choose your own keys.

(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(add-hook 'org-mode-hook 'turn-on-font-lock) ; not needed when global-font-lock-mode is on
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

;;;; ___________________________________________________________________________
;;;; Personal tailoring

;;;; ---------------------------------------------------------------------------
;;;; General

(setq org-directory
      "~/Documents/jsk/development-100/__for-sync/to-do-and-planning")
(setq org-replace-disputed-keys t)
(setq org-log-done nil)

(setq org-return-follows-link t)

;;;; ---------------------------------------------------------------------------
;;;; Priorities

(setq org-highest-priority ?1)
(setq org-lowest-priority  ?9)
(setq org-default-priority ?6)

;;;; ---------------------------------------------------------------------------
;;;; Layout

(add-hook 'org-mode-hook
          (lambda ()
            (linum-mode 0) ; see "Linum-mode + org-indent-mode gives strange graphical refresh bugs" at http://orgmode.org/worg/org-issues.html
            ;; (setq org-indent-fix-section-after-idle-time nil)
            (setq org-indent-indentation-per-level 3) ; the default of 2 is too small; 4 screws up auto indentation in large files
            ;; (setq org-indent-max 60)
            ;; (setq org-indent-max-levels 80)
            ))

;;;; ---------------------------------------------------------------------------
;;;; Copying and pasting

(add-hook 'org-mode-hook
          (lambda ()
            (setq org-yank-adjusted-subtrees t)))

;;;; ---------------------------------------------------------------------------
;;;; Exporting

(add-hook 'org-mode-hook
          (lambda ()
            ;; (setq org-export-headline-levels 3)
            (setq org-export-initial-scope 'subtree)))


;;;; ---------------------------------------------------------------------------
;;;; Scrolling

(add-hook 'org-mode-hook
          (lambda ()
            (setq org-cycle-hook
                  (remq 'org-optimize-window-after-visibility-change
                        org-cycle-hook))))

;;;; ---------------------------------------------------------------------------
;;;; Dependencies

(setq org-enforce-todo-dependencies t)
(setq org-agenda-dim-blocked-tasks 'invisible)

;;;; ---------------------------------------------------------------------------
;;;; Capture

(setq org-default-notes-file (concat org-directory "/___notes.org"))
(define-key global-map "\C-cc" 'org-capture)

;;;; ---------------------------------------------------------------------------
;;;; Refiling

;; (progn org-use-fast-todo-selection)

;; (setf org-refile-targets '((nil :maxlevel . 9)
;;                            (org-agenda-files :maxlevel . 9)))
;; (setf org-refile-use-outline-path t)
;; (setf org-outline-path-complete-in-steps t)
;; (setq org-refile-allow-creating-parent-nodes 'confirm)
;; (setq org-completion-use-ido nil)

;;;; ---------------------------------------------------------------------------
;;;; Agendas

(setq org-agenda-files (list org-directory))

;;; From http://orgmode.org/worg/org-faq.html
;;;   How can I stop the mouse cursor from highlighting lines in the agenda?
;;;     You can add the following to your .emacs:

(add-hook 'org-finalize-agenda-hook
    (lambda () (remove-text-properties
           (point-min) (point-max) '(mouse-face t))))
