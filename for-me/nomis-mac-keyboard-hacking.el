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
;;;; ---- A few conveniences that don't change existing functonality ----

(progn
  ;; Deal with delete-frame.
  ;; The default C-x 5 0 is too long.
  ;; Can't use the normal M-w without stealing from Emacs.
  ;; This is ok:
  (define-key global-map (kbd "M-W") 'delete-frame))

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

  (defparameter *nomis-meta-w-replacement-p* nil)

  (defun nomis-meta-w (arg)
    (interactive "p")
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

(progn
  ;; Deal with M-z.
  ;; The default for M-z is zap-to-char.
  ;; I never use M-z, so I don't need a new binding for zap-to-char.
  (define-key global-map (kbd "M-z") 'undo))

(progn
  ;; Deal with M-x.

  (defparameter *nomis-meta-x-replacement* nil ; 'ignore-until-i-learn-not-to-use-it
    "Whether to do nomis M-x replacement.
nil
  means no.
ignore-until-i-learn-not-to-use-it
  means issue a warning message that nothing is going to happen
any other value
  means do nomis M-x replacement.")
  
  (defun issue-meta-x-replacement-message ()
    (message "Use Option-x or Option-z instead of M-x")
    (ding))
  
  (defun nomis-meta-x (arg)
    (interactive "p")
    (cond
     ((null *nomis-meta-x-replacement*)
      (smex))
     ((eql *nomis-meta-x-replacement* 'ignore-until-i-learn-not-to-use-it)
      (issue-meta-x-replacement-message))
     (t
      (kill-region (point) (mark)))))
  
  (defun nomis-option-x (arg)
    (interactive "p")
    (if (null *nomis-meta-x-replacement*)
        (insert "≈")
      (smex)))
  
  (defun nomis-option-z (arg)
    (interactive "p")
    (if (null *nomis-meta-x-replacement*)
        (insert "Ω")
      (smex)))
  
  (define-key global-map (kbd "M-x") 'nomis-meta-x)
  
  (progn
    ;; Option-x and Option-z for the thing that is usually M-x.
    ;; Option-x might seem more natural, but typing it requires nasty
    ;; finger-bending.
    (define-key global-map "≈" 'nomis-option-x)
    (define-key global-map "Ω" 'nomis-option-z)))

;;;; ___________________________________________________________________________

(provide 'nomis-mac-keyboard-hacking)
