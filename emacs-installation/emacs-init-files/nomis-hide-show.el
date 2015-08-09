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

(defun nomis-hs-hydra-adjust-level (n)
  (setq nomis-hs-hydra-level (max 1
                            (+ nomis-hs-hydra-level n)))
  (hs-hide-level nomis-hs-hydra-level))

(defun nomis-hs-hydra-init ()
  (interactive)
  (setq nomis-hs-hydra-level 1)
  (nomis-hs-hydra-adjust-level 0))

(defun nomis-hs-less ()
  (interactive)
  (nomis-hs-hydra-adjust-level -1))

(defun nomis-hs-more ()
  (interactive)
  (nomis-hs-hydra-adjust-level 1))

(defhydra nomis-hs-hydra
  (global-map "H-q H-q")
  "Hide-show incremental"
  ("H-q" nomis-hs-hydra-init "Init")
  ("H-." nomis-hs-less "Less")
  ("H-/" nomis-hs-more "More")
  ("." nomis-hs-less "Less")
  ("/" nomis-hs-more "More"))

(provide 'nomis-hide-show)
