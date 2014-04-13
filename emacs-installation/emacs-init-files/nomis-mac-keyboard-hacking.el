;;;; Init stuff -- Mac keyboard hacking.

;;;; ___________________________________________________________________________
;;;; ---- # key ----

;; This is not good -- can't type a "#" when doing a C-s search.  (Typing
;; a "#" exits the search.)

;; (global-set-key (kbd "M-£") '(lambda () (interactive) (insert "#")))

;;;; ___________________________________________________________________________
;;;; ---- I think this does all you need ----
;;;;
;;;; Found at http://www.emacswiki.org/emacs/MetaKeyProblems.

(setq mac-command-modifier 'meta) ; map command key to meta
(setq mac-option-modifier nil)    ; do not map option key

;;;; ___________________________________________________________________________
;;;; ---- Experiment: Make some keys more "normal" ----

;;;; The only thing I actually need here is the binding of M-v to yank, so that
;;;; I can paste from Alfred. (Fortunately I don't need the default of M-v for
;;;; scroll-down-command.)

;;;; The rest is an experiment.

(progn
  ;; Deal with M-v.
  ;; Needed for Alfred.
  ;; Don't worry about losing M-v for scroll-down-command -- you have Page Up.
  (define-key global-map (kbd "M-v") 'yank))

(progn
  ;; Deal with M-c.
  ;; The default for M-c is capitalize-word.
  ;; I'm used to using M-C for capitalize-word (maybe from LW), but that only
  ;; works in Emacs because of "translated from" keystrokes that try to work
  ;; around the fact that you might have Caps Lock on accidentally.
  (define-key global-map (kbd "M-c") 'kill-ring-save)
  (define-key global-map (kbd "M-C") 'capitalize-word))

(progn
  ;; Deal with M-w.
  ;; The default for M-w is kill-ring-save.
  ;; Consider using M-c for that and having M-w delete a frame.

  (defvar *nomis-meta-w-replacement-p* nil)

  (defun nomis-meta-w ()
    (interactive)
    (cl-flet ((nomis-meta-w-replacement
               ()
               (ding)
               (when (prog1
                         (y-or-n-p "Really delete frame?")
                       (message nil))
                 (delete-frame))))
      (if *nomis-meta-w-replacement-p*
          (nomis-meta-w-replacement)
        (kill-ring-save (point) (mark)))))
  
  (define-key global-map (kbd "M-w") 'nomis-meta-w))

;; Nope -- see "nomis-undo-tree.el".
;; (progn
;;   ;; Deal with M-z.
;;   ;; The default for M-z is zap-to-char.  I don't need that, so...
;;   (define-key global-map (kbd "M-z") 'undo))

(progn
  ;; Deal with M-x.

  (defvar *nomis-meta-x-replacement* nil ; 'ignore-until-i-learn-not-to-use-it
    "Whether to do nomis M-x replacement.
nil
  means no.
ignore-until-i-learn-not-to-use-it
  means issue a warning message that nothing is going to happen
any other value
  means do nomis M-x replacement.")

  (defvar *nomis-meta-x-command-when-first-loaded*
    ;; Note that this relies on any non-build-in binding of M-x being set
    ;; before this file is loaded. Hmmm, not great.
    (let ((command (key-binding (kbd "M-x"))))
      (if (and (boundp '*nomis-meta-x-command-when-first-loaded*)
               (eql command 'nomis-meta-x))
          ;; Don't change what happened when first loaded.
          *nomis-meta-x-command-when-first-loaded*
        command)))
  
  (defun call-old-meta-x (arg)
    (case *nomis-meta-x-command-when-first-loaded*
      ('execute-extended-command
       (execute-extended-command arg))
      ('smex
       (smex))
      (t
       (error "Don't know how to call %s"
              *nomis-meta-x-command-when-first-loaded*))))
  
  (defun issue-meta-x-replacement-message ()
    (message "Use Option-x or Option-z instead of M-x")
    (ding))
  
  (defun nomis-meta-x (arg)
    (interactive "p")
    (cond
     ((null *nomis-meta-x-replacement*)
      (call-old-meta-x arg))
     ((eql *nomis-meta-x-replacement* 'ignore-until-i-learn-not-to-use-it)
      (issue-meta-x-replacement-message))
     (t
      (kill-region (point) (mark)))))
  
  (defun nomis-option-x (arg)
    (interactive "p")
    (if (null *nomis-meta-x-replacement*)
        (insert "≈")
      (call-old-meta-x arg)))
  
  (defun nomis-option-z (arg)
    (interactive "p")
    (if (null *nomis-meta-x-replacement*)
        (insert "Ω")
      (call-old-meta-x arg)))
  
  (define-key global-map (kbd "M-x") 'nomis-meta-x)
  
  (progn
    ;; Option-x and Option-z for the thing that is usually M-x.
    ;; Option-x might seem more natural, but typing it requires nasty
    ;; finger-bending.
    (define-key global-map "≈" 'nomis-option-x)
    (define-key global-map "Ω" 'nomis-option-z)))

;;;; ___________________________________________________________________________

(provide 'nomis-mac-keyboard-hacking)
