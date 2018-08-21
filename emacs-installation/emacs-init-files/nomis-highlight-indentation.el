;;;; nomis-highlight-indentation.el --- nomis highlight-indentation tailoring ---  -*- lexical-binding: t -*-

(defun nomis/setup-highlight-indentation-mode ()
  ;; From https://github.com/antonj/Highlight-Indentation-for-Emacs
  (set-face-background 'highlight-indentation-face "#e3e3d3"))

(defun nomis/setup-highlight-indentation-current-column-mode ()
  ;; From https://github.com/antonj/Highlight-Indentation-for-Emacs
  (set-face-background 'highlight-indentation-current-column-face "#c3b3b3"))

(add-hook 'highlight-indentation-mode-hook
          'nomis/setup-highlight-indentation-mode)

(add-hook 'highlight-indentation-current-column-mode-hook
          'nomis/setup-highlight-indentation-current-column-mode)

(defun nomis/toggle-fold-indentation ()
  ;; From https://stackoverflow.com/questions/1587972/how-to-display-indentation-guides-in-emacs/4459159#4459159
  ;; (and renamed).
  "Toggle fold all lines larger than indentation on current line"
  (interactive)
  (let ((col 1))
    (save-excursion
      (back-to-indentation)
      (setq col (+ 1 (current-column)))
      (set-selective-display
       (if selective-display nil (or col 1))))))

;;;; TODO You have integrated this with `nomis/hs-toggle-hiding`.
;;;;      Can you add more functionality here, so that you can hide/show
;;;;      a single chunk rather than having the same level across a whole
;;;;      buffer?

(provide 'nomis-highlight-indentation)
