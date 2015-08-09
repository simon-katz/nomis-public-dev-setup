;;;; Init stuff -- nomis-hide-show

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

(require 'key-chord)
(key-chord-mode 1)

(key-chord-define-global "q[" 'hs-hide-all)
(key-chord-define-global "q]" 'hs-show-all)
(key-chord-define-global "q;" 'nomis-hs-hide-block)
(key-chord-define-global "q'" 'nomis-hs-show-block)
(key-chord-define-global "q/" 'nomis-hs-toggle-hiding)

(defun display-code-line-counts (ov)
  (when (eq 'code (overlay-get ov 'hs))
    (overlay-put ov 'help-echo
                 (buffer-substring (overlay-start ov)
                                   (overlay-end ov)))))

(setq hs-set-up-overlay 'display-code-line-counts)

(defadvice goto-line (after expand-after-goto-line
                            activate compile)
  "hideshow-expand affected block when using goto-line in a collapsed buffer"
  (save-excursion
    (hs-show-block)))

(provide 'nomis-hide-show)
