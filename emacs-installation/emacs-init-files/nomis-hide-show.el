;;;; Init stuff -- nomis-hide-show

;;;; ___________________________________________________________________________

(defun nomis-hs-hide-block ()
  (interactive)
  (hs-hide-block)
  (backward-char))

(defun nomis-hs-show-block ()
  (interactive)
  (hs-show-block)
  (backward-char))

(defun nomis-hs-toggle-hiding ()
  (interactive)
  (hs-toggle-hiding)
  (backward-char))

(define-key global-map (kbd "H-q H-[") 'hs-hide-all)
(define-key global-map (kbd "H-q H-]") 'hs-show-all)
(define-key global-map (kbd "H-q H-;") 'nomis-hs-hide-block)
(define-key global-map (kbd "H-q H-'") 'nomis-hs-show-block)
(define-key global-map (kbd "H-q H-/") 'nomis-hs-toggle-hiding)

(key-chord-define-global "q[" 'hs-hide-all)
(key-chord-define-global "q]" 'hs-show-all)
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
;;;; nomis-hs-hydra

(require 'hydra)

(defvar nomis-hs-hydra-level)

(defun nomis-hs-hydra-set-level (n)
  (setq nomis-hs-hydra-level n)
  (hs-hide-level nomis-hs-hydra-level))

(defun nomis-hs-hydra-adjust-level (n)
  (setq nomis-hs-hydra-level (max 1
                                  (+ nomis-hs-hydra-level n)))
  (nomis-hs-hydra-set-level nomis-hs-hydra-level))

(defun nomis-hs-hydra-init ()
  (interactive)
  (nomis-hs-hydra-set-level 1))

(defun nomis-hs-less ()
  (interactive)
  (nomis-hs-hydra-adjust-level -1))

(defun nomis-hs-more ()
  (interactive)
  (nomis-hs-hydra-adjust-level 1))

(defun nomis-hs-set-1 ()
  (interactive)
  (nomis-hs-hydra-set-level 1))

(defun nomis-hs-set-2 ()
  (interactive)
  (nomis-hs-hydra-set-level 2))

(defun nomis-hs-set-3 ()
  (interactive)
  (nomis-hs-hydra-set-level 3))

(defun nomis-hs-set-4 ()
  (interactive)
  (nomis-hs-hydra-set-level 4))

(defun nomis-hs-set-5 ()
  (interactive)
  (nomis-hs-hydra-set-level 5))

(defun nomis-hs-set-6 ()
  (interactive)
  (nomis-hs-hydra-set-level 6))

(defun nomis-hs-set-7 ()
  (interactive)
  (nomis-hs-hydra-set-level 7))

(defun nomis-hs-set-8 ()
  (interactive)
  (nomis-hs-hydra-set-level 8))

(defun nomis-hs-set-9 ()
  (interactive)
  (nomis-hs-hydra-set-level 9))

(defun nomis-hs-set-10 ()
  (interactive)
  (nomis-hs-hydra-set-level 10))

(defhydra nomis-hs-hydra
  (global-map "H-q H-q")
  "Hide-show incremental"
  ("H-q"     nomis-hs-hydra-init "Init")
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
  ("-"       nomis-hs-less       "Less")
  ("="       nomis-hs-more       "More")
  ("<left>"  nomis-hs-less       "Less")
  ("<right>" nomis-hs-more       "More")
  ("_"       nomis-hs-set-1      "1 level")
  ("+"       nomis-hs-show-block "Show all"))

(provide 'nomis-hide-show)
