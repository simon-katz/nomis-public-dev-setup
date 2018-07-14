;;;; Init stuff -- nomis-hide-show

;;;; ___________________________________________________________________________

(require 'nomis-key-chord)
(require 'nomis-hydra)

;;;; ___________________________________________________________________________

(defun nomis-hs-hide-all ()
  (interactive)
  (hs-minor-mode 1)
  (hs-hide-all)
  (nomis-beginning-of-top-level-form) ; without this, a following show command does not-very-nice positioning of the cursor
  )

(defun nomis-hs-show-all ()
  (interactive)
  (hs-minor-mode 1)
  (hs-show-all))

(defun nomis-hs-hide-block ()
  (interactive)
  (hs-minor-mode 1)
  (hs-hide-block)
  (backward-char))

(defun nomis-hs-show-block ()
  (interactive)
  (hs-minor-mode 1)
  (hs-show-block)
  (backward-char))

(defun nomis-hs-toggle-hiding ()
  (interactive)
  (hs-minor-mode 1)
  (hs-toggle-hiding)
  (backward-char))

(define-key global-map (kbd "H-q H--")  'nomis-hs-hide-all)
(define-key global-map (kbd "H-q H-[")  'nomis/hs-adjust/set-0)
(define-key global-map (kbd "H-q H-'")  'nomis/hs-adjust/less)
(define-key global-map (kbd "H-q H-l")  'nomis/hs-adjust/set-level)
(define-key global-map (kbd "H-q H-\\") 'nomis/hs-adjust/more)
(define-key global-map (kbd "H-q H-]")  'nomis/hs-adjust/show-all)
(define-key global-map (kbd "H-q H-=")  'nomis-hs-show-all)
(define-key global-map (kbd "H-q H-/")  'nomis-hs-toggle-hiding)
(define-key global-map (kbd "H-q H-0")  'nomis/hs-adjust/set-level/0)
(define-key global-map (kbd "H-q H-1")  'nomis/hs-adjust/set-level/1)
(define-key global-map (kbd "H-q H-2")  'nomis/hs-adjust/set-level/2)
(define-key global-map (kbd "H-q H-3")  'nomis/hs-adjust/set-level/3)
(define-key global-map (kbd "H-q H-4")  'nomis/hs-adjust/set-level/4)
(define-key global-map (kbd "H-q H-5")  'nomis/hs-adjust/set-level/5)
(define-key global-map (kbd "H-q H-6")  'nomis/hs-adjust/set-level/6)
(define-key global-map (kbd "H-q H-7")  'nomis/hs-adjust/set-level/7)
(define-key global-map (kbd "H-q H-8")  'nomis/hs-adjust/set-level/8)
(define-key global-map (kbd "H-q H-9")  'nomis/hs-adjust/set-level/9)

(key-chord-define-global "q-"  'nomis-hs-hide-all)
(key-chord-define-global "q["  'nomis/hs-adjust/set-0)
(key-chord-define-global "q'"  'nomis/hs-adjust/less)
(key-chord-define-global "ql"  'nomis/hs-adjust/set-level)
(key-chord-define-global "q\\" 'nomis/hs-adjust/more)
(key-chord-define-global "q]"  'nomis/hs-adjust/show-all)
(key-chord-define-global "q="  'nomis-hs-show-all)
(key-chord-define-global "q/"  'nomis-hs-toggle-hiding)
(key-chord-define-global "q0"  'nomis/hs-adjust/set-level/0)
(key-chord-define-global "q1"  'nomis/hs-adjust/set-level/1)
(key-chord-define-global "q2"  'nomis/hs-adjust/set-level/2)
(key-chord-define-global "q3"  'nomis/hs-adjust/set-level/3)
(key-chord-define-global "q4"  'nomis/hs-adjust/set-level/4)
(key-chord-define-global "q5"  'nomis/hs-adjust/set-level/5)
(key-chord-define-global "q6"  'nomis/hs-adjust/set-level/6)
(key-chord-define-global "q7"  'nomis/hs-adjust/set-level/7)
(key-chord-define-global "q8"  'nomis/hs-adjust/set-level/8)
(key-chord-define-global "q9"  'nomis/hs-adjust/set-level/9)

(defun nomis-display-hs-hidden-stuff (ov)
  (when (eq 'code (overlay-get ov 'hs))
    (overlay-put ov 'help-echo
                 (buffer-substring (overlay-start ov)
                                   (overlay-end ov)))
    (overlay-put ov 'display
                 (propertize (format "......... / %d"
                                     (count-lines (overlay-start ov)
                                                  (overlay-end ov)))
                             'face 'font-lock-type-face))))

(setq hs-set-up-overlay 'nomis-display-hs-hidden-stuff)

(defadvice goto-line (after expand-after-goto-line
                            activate compile)
  "hideshow-expand affected block when using goto-line in a collapsed buffer"
  (save-excursion
    (hs-minor-mode 1)
    (hs-show-block)))

;;;; ___________________________________________________________________________
;;;; nomis/hs-adjust

(defvar nomis/hs-adjust/level 0)

(defun nomis/hs-adjust/set-level (n)
  (interactive "p")
  (setq nomis/hs-adjust/level n)
  (if (zerop n)
      (nomis-hs-hide-block)
    (hs-hide-level nomis/hs-adjust/level)))

(defun nomis/hs-adjust/set-level/0 ()
  (interactive)
  (nomis/hs-adjust/set-level 0))

(defun nomis/hs-adjust/set-level/1 ()
  (interactive)
  (nomis/hs-adjust/set-level 1))

(defun nomis/hs-adjust/set-level/2 ()
  (interactive)
  (nomis/hs-adjust/set-level 2))

(defun nomis/hs-adjust/set-level/3 ()
  (interactive)
  (nomis/hs-adjust/set-level 3))

(defun nomis/hs-adjust/set-level/4 ()
  (interactive)
  (nomis/hs-adjust/set-level 4))

(defun nomis/hs-adjust/set-level/5 ()
  (interactive)
  (nomis/hs-adjust/set-level 5))

(defun nomis/hs-adjust/set-level/6 ()
  (interactive)
  (nomis/hs-adjust/set-level 6))

(defun nomis/hs-adjust/set-level/7 ()
  (interactive)
  (nomis/hs-adjust/set-level 7))

(defun nomis/hs-adjust/set-level/8 ()
  (interactive)
  (nomis/hs-adjust/set-level 8))

(defun nomis/hs-adjust/set-level/9 ()
  (interactive)
  (nomis/hs-adjust/set-level 9))

(defun nomis/hs-adjust/inc-level (n)
  (setq nomis/hs-adjust/level (max 0
                                   (+ nomis/hs-adjust/level n)))
  (nomis/hs-adjust/set-level nomis/hs-adjust/level))

(defun nomis/hs-adjust/init ()
  (interactive)
  (hs-minor-mode 1)
  ;; Set to last-set level. Useful when working on the same form a second time.
  (nomis/hs-adjust/set-level nomis/hs-adjust/level))

(defun nomis/hs-adjust/less (n)
  (interactive "p")
  (nomis/hs-adjust/inc-level (- n)))

(defun nomis/hs-adjust/more (n)
  (interactive "p")
  (nomis/hs-adjust/inc-level n))

(defun nomis/hs-adjust/set-0 ()
  (interactive)
  (nomis/hs-adjust/set-level 0))

(defun nomis/hs-adjust/set-0/and-exit ()
  ;; This exists to overcome a bug in Hydra when you have both
  ;;     :exit t
  ;; and
  ;;     :exit nil
  ;; for the same function.
  (interactive)
  (nomis/hs-adjust/set-0))

(defun nomis/hs-adjust/show-all ()
  (interactive)
  ;; This exists to overcome a bug when showing all when level shown is 1,
  ;; whereby the cursor moved weirdly and fucked things up.
  (nomis-hs-hide-block)
  (nomis-hs-show-block))

(defun nomis/hs-adjust/show-all/and-exit ()
  ;; This exists to overcome a bug in Hydra when you have both
  ;;     :exit t
  ;; and
  ;;     :exit nil
  ;; for the same function.
  (interactive)
  (nomis/hs-adjust/show-all))

(define-nomis-hydra nomis/hs-adjust
  :name-as-string "Hide-show incremental"
  :key "H-q H-q"
  :vars (nomis/hs-adjust/saved-level)
  :init-form    (progn
                  (setq nomis/hs-adjust/saved-level nomis/hs-adjust/level)
                  (nomis/hs-adjust/init))
  :cancel-form (nomis/hs-adjust/set-level nomis/hs-adjust/saved-level)
  :hydra-heads
  (("-"  nomis-hs-hide-all         "Hide All")
   ("["  nomis/hs-adjust/set-0     "Hide this form")
   ("'"  nomis/hs-adjust/less      "Less")
   ("\\" nomis/hs-adjust/more      "More")
   ("]"  nomis/hs-adjust/show-all  "Show this form")
   ("="  nomis-hs-show-all         "Show All")
   ("/"  nomis-hs-toggle-hiding    "Toggle")
   ("0"  nomis/hs-adjust/set-level/0)
   ("1"  nomis/hs-adjust/set-level/1)
   ("2"  nomis/hs-adjust/set-level/2)
   ("3"  nomis/hs-adjust/set-level/3)
   ("4"  nomis/hs-adjust/set-level/4)
   ("5"  nomis/hs-adjust/set-level/5)
   ("6"  nomis/hs-adjust/set-level/6)
   ("7"  nomis/hs-adjust/set-level/7)
   ("8"  nomis/hs-adjust/set-level/8)
   ("9"  nomis/hs-adjust/set-level/9)))

(provide 'nomis-hide-show)
