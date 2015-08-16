;;;; Init stuff -- nomis-hide-show

;;;; ___________________________________________________________________________

(defun nomis-hs-hide-all ()
  (interactive)
  (hs-minor-mode 1)
  (hs-hide-all))

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

(define-key global-map (kbd "H-q H-[") 'nomis-hs-hide-all)
(define-key global-map (kbd "H-q H-]") 'nomis-hs-show-all)
(define-key global-map (kbd "H-q H-;") 'nomis-hs-hide-block)
(define-key global-map (kbd "H-q H-'") 'nomis-hs-show-block)
(define-key global-map (kbd "H-q H-/") 'nomis-hs-toggle-hiding)

(key-chord-define-global "q[" 'nomis-hs-hide-all)
(key-chord-define-global "q]" 'nomis-hs-show-all)
(key-chord-define-global "q;" 'nomis-hs-hide-block)
(key-chord-define-global "q'" 'nomis-hs-show-block)
(key-chord-define-global "q/" 'nomis-hs-toggle-hiding)

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
    (hs-show-block)))

;;;; ___________________________________________________________________________
;;;; nomis-hs-adjust

(defvar nomis-hs-level)

(defun nomis-hs-set-level (n)
  (setq nomis-hs-level n)
  (hs-hide-level nomis-hs-level))

(defun nomis-hs-inc-level (n)
  (setq nomis-hs-level (max 1
                            (+ nomis-hs-level n)))
  (nomis-hs-set-level nomis-hs-level))

(defun nomis-hs-adjust-init ()
  (interactive)
  (hs-minor-mode 1)
  (nomis-hs-set-level 1))

(defun nomis-hs-less ()
  (interactive)
  (nomis-hs-inc-level -1))

(defun nomis-hs-more ()
  (interactive)
  (nomis-hs-inc-level 1))

(defun nomis-hs-set-1 ()
  (interactive)
  (nomis-hs-set-level 1))

(defun nomis-hs-set-2 ()
  (interactive)
  (nomis-hs-set-level 2))

(defun nomis-hs-set-3 ()
  (interactive)
  (nomis-hs-set-level 3))

(defun nomis-hs-set-4 ()
  (interactive)
  (nomis-hs-set-level 4))

(defun nomis-hs-set-5 ()
  (interactive)
  (nomis-hs-set-level 5))

(defun nomis-hs-set-6 ()
  (interactive)
  (nomis-hs-set-level 6))

(defun nomis-hs-set-7 ()
  (interactive)
  (nomis-hs-set-level 7))

(defun nomis-hs-set-8 ()
  (interactive)
  (nomis-hs-set-level 8))

(defun nomis-hs-set-9 ()
  (interactive)
  (nomis-hs-set-level 9))

(defun nomis-hs-set-10 ()
  (interactive)
  (nomis-hs-set-level 10))

(require 'hydra)

(defhydra nomis-hs-adjust
  (global-map "H-q H-q")
  "Hide-show incremental"
  ("H-q"     nomis-hs-adjust-init "Init")
  ("-"       nomis-hs-less       "Less")
  ("<left>"  nomis-hs-less       "Less")
  ("_"       nomis-hs-set-1      "1 level")
  ("1"       nomis-hs-set-1      "1 level")
  ("2"       nomis-hs-set-2      "2 levels")
  ("3"       nomis-hs-set-3      "3 levels")
  ("4"       nomis-hs-set-4      "4 levels")
  ("5"       nomis-hs-set-5      "5 levels")
  ("6"       nomis-hs-set-6      "6 levels")
  ("7"       nomis-hs-set-7      "7 levels")
  ("8"       nomis-hs-set-8      "8 levels")
  ("9"       nomis-hs-set-9      "9 levels")
  ("0"       nomis-hs-set-10     "10 levels")
  ("="       nomis-hs-more       "More")
  ("<right>" nomis-hs-more       "More")
  ("+"       nomis-hs-show-block "Show all"))

(provide 'nomis-hide-show)
