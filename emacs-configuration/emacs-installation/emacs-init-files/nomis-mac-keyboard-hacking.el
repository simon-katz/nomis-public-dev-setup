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

(setq mac-command-modifier 'meta)  ; map command key to meta
(setq mac-option-modifier nil)     ; do not map option key
(setq ns-function-modifier 'hyper) ; set Mac's Fn key to Hyper

(setq w32-lwindow-modifier 'hyper) ; set Window's Left Windows key to Hyper

;;;; ___________________________________________________________________________

(provide 'nomis-mac-keyboard-hacking)
