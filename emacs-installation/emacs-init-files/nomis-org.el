;;;; Init stuff -- Org Mode.

;;;; ___________________________________________________________________________
;;;; ___________________________________________________________________________
;;;; The following lines are always needed. Choose your own keys.

(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(add-hook 'org-mode-hook 'turn-on-font-lock) ; not needed when global-font-lock-mode is on
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

;;;; ___________________________________________________________________________
;;;; ___________________________________________________________________________
;;;; Personal tailoring

;;;; ___________________________________________________________________________
;;;; General

(defvar nomis-notes-directory
  (if i-am-nomis-p
      (concat (nomis-load-file-directory)
              "../../../notes")
    "you are not nomis so you don't have a nomis-notes-directory"))

(setq org-directory
      (if i-am-nomis-p
          (concat nomis-notes-directory "/to-do-and-planning")
        "~/.emacs-org-dir"))


(setq org-replace-disputed-keys t)
(setq org-log-done nil)

(setq org-return-follows-link t)

(setq org-startup-indented t)

;;;; ___________________________________________________________________________
;;;; Priorities

(setq org-highest-priority ?1)
(setq org-lowest-priority  ?9)
(setq org-default-priority ?2)

;;;; ___________________________________________________________________________
;;;; Org mode hook function

(defun nomis-org-mode ()
  ;; Layout
  (linum-mode 0) ; see "Linum-mode + org-indent-mode gives strange graphical refresh bugs" at http://orgmode.org/worg/org-issues.html
  ;; (setq org-indent-fix-section-after-idle-time nil)
  (setq org-indent-indentation-per-level 3) ; the default of 2 is too small; 4 screws up auto indentation in large files
  ;; (setq org-indent-max 60)
  ;; (setq org-indent-max-levels 80)

  ;; Copying and pasting
  (setq org-yank-adjusted-subtrees t)
  
  ;; Exporting
  ;; (setq org-export-headline-levels 3)
  (setq org-export-initial-scope 'subtree)

  ;; Scrolling
  (setq org-cycle-hook
        (remq 'org-optimize-window-after-visibility-change
              org-cycle-hook)))

(add-hook 'org-mode-hook 'nomis-org-mode)

;;;; ___________________________________________________________________________
;;;; Dependencies

(setq org-enforce-todo-dependencies t)
;; (setq org-agenda-dim-blocked-tasks 'invisible) ; actually the default dimmimg is nice -- you can see more info

;;;; ___________________________________________________________________________
;;;; Capture

(setq org-default-notes-file (concat org-directory "/___notes.org"))
(define-key global-map "\C-cc" 'org-capture)

;;;; ___________________________________________________________________________
;;;; Refiling

;; (progn org-use-fast-todo-selection)

;; (setf org-refile-targets '((nil :maxlevel . 9)
;;                            (org-agenda-files :maxlevel . 9)))
;; (setf org-refile-use-outline-path t)
;; (setf org-outline-path-complete-in-steps t)
;; (setq org-refile-allow-creating-parent-nodes 'confirm)
;; (setq org-completion-use-ido nil)

;;;; ___________________________________________________________________________
;;;; Agendas

(setq org-agenda-files
      (if i-am-nomis-p
          (progn
            (load-library "find-lisp")
            (find-lisp-find-files nomis-notes-directory
                                  "\.org$"))
        (list org-directory)))

;;; From http://orgmode.org/worg/org-faq.html
;;;   How can I stop the mouse cursor from highlighting lines in the agenda?
;;;     You can add the following to your .emacs:

(add-hook 'org-finalize-agenda-hook
          (lambda () (remove-text-properties
                      (point-min) (point-max) '(mouse-face t))))

(progn
  ;; I want to train myself not to hit RETURN when in an agenda;
  ;; hit SPACE instead to show in other window.
  (defadvice org-agenda-switch-to (around nomis-org-agenda-switch-to (&rest args))
    (message "Use org-agenda-show-and-scroll-up (SPACE) instead")
    (beep)
    ;; ad-do-it
    )
  (ad-activate 'org-agenda-switch-to))

;;;; ___________________________________________________________________________
;;;; Fontify code in code blocks

(setq org-src-fontify-natively t)

;;;; ___________________________________________________________________________

(provide 'nomis-org)
