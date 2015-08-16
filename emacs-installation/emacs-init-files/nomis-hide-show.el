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

(defun nomis-hs-less (n)
  (interactive "p")
  (nomis-hs-inc-level -1))

(defun nomis-hs-more (n)
  (interactive "p")
  (nomis-hs-inc-level 1))

(defun nomis-hs-set-1 ()
  (interactive)
  (nomis-hs-set-level 1))

(require 'nomis-hydra)

(define-nomis-hydra nomis/hs-adjust
  :name-as-string "Hide-show incremental"
  :key "H-q H-q"
  :init-form   (nomis-hs-adjust-init)
  :hydra-heads (("-"         nomis-hs-less       "Less")
                ("<left>"    nomis-hs-less       "Less")
                ("<S-left>"  nomis-hs-set-1      "1 level")
                ("_"         nomis-hs-set-1      "1 level")
                ("="         nomis-hs-more       "More")
                ("<right>"   nomis-hs-more       "More")
                ("<S-right>" nomis-hs-show-block "Show all")
                ("+"         nomis-hs-show-block "Show all")))

(provide 'nomis-hide-show)
