;;;; ________ * Init stuff -- Org Mode.

;;;; ________ ** Require things

(progn
  (setq org-replace-disputed-keys t) ; must be done before requiring org
  (require 'org))

(require 'cl)

;;;; ________ ** Stuff everyone needs

;;; The following lines are always needed. Choose your own keys.

(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(add-hook 'org-mode-hook 'turn-on-font-lock) ; not needed when global-font-lock-mode is on
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

;;;; ________ ** Personal tailoring

;;;; ________ *** General

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

;;;; ________ *** Priorities

(setq org-highest-priority ?1)
(setq org-lowest-priority  ?9)
(setq org-default-priority ?2)

;;;; ________ *** Org mode hook function

(defun nomis-org-mode ()
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
  (setq org-export-initial-scope 'subtree)

  ;; Scrolling
  (setq org-cycle-hook
        (remq 'org-optimize-window-after-visibility-change
              org-cycle-hook)))

(add-hook 'org-mode-hook 'nomis-org-mode)

;;;; ________ *** Dependencies

(setq org-enforce-todo-dependencies t)
;; (setq org-agenda-dim-blocked-tasks 'invisible) ; actually the default dimmimg is nice -- you can see more info

;;;; ________ *** Capture

(setq org-default-notes-file
      (concat org-directory
              "/___captured-notes-from-"
              (substring (symbol-name nomis/system-name)
                         1)
              ".org"))

(define-key global-map "\C-cc" 'org-capture)

;;;; ________ *** Refiling

;;;; - I haven't quite got this nice (or maybe I did).
;;;; - BUT:
;;;;   After a refile, undo only undoes what happened in one buffer, even
;;;;   though two buffers have been modified. That's crappy.

;;;; ________ **** Play #1

;; (progn org-use-fast-todo-selection)

;; (setf org-refile-targets '((nil :maxlevel . 9)
;;                            (org-agenda-files :maxlevel . 9)))
;; (setf org-refile-use-outline-path t)
;; (setf org-outline-path-complete-in-steps t)
;; (setq org-refile-allow-creating-parent-nodes 'confirm)
;; (setq org-completion-use-ido nil)

;;;; ________ **** Play #2 -- 2017-08-10

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

;;;; ________ *** Agendas

(require 'org-agenda)

(progn
  (defun nomis-org-reset-org-agenda-files ()
    (interactive)
    (setq org-agenda-files
          (progn
            (load-library "find-lisp")
            (find-lisp-find-files org-directory
                                  "\.org$"))))
  (defadvice org-todo-list (before reset-agenda-files
                                   activate compile)
    (nomis-org-reset-org-agenda-files)))

(progn
  (defun nomis-org-finalize-agenda-hook ()
    (hl-line-mode)
    ;; From http://orgmode.org/worg/org-faq.html
    ;;   How can I stop the mouse cursor from highlighting lines
    ;;   in the agenda?
    ;;     You can add the following to your .emacs:
    (remove-text-properties
     (point-min) (point-max) '(mouse-face t)))
  (add-hook 'org-finalize-agenda-hook
            'nomis-org-finalize-agenda-hook))

(progn
  (defun nomis-setup-org-keys ()
    ;; I don't like RETURN in org agenda giving ORG-AGENDA-SWITCH-TO.
    ;; I prefer this:
    (org-defkey org-agenda-mode-map "\C-m" 'org-agenda-show-and-scroll-up))
  (add-hook 'org-mode-hook 'nomis-setup-org-keys))

(add-hook 'org-mode-hook 'nomis-turn-on-idle-highlight-mode)

;;;; ________ *** Fontify code in code blocks

(setq org-src-fontify-natively t)

;;;; ________ *** orgstruct

;; To use, enable orgstruct-mode or orgstruct++-mode

(setq orgstruct-heading-prefix-regexp ";* *_* ")

;;;; ________ *** Display

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

;;;; ________ ** end

(provide 'nomis-org)
