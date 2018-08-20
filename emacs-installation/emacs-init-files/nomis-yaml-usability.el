;;;; nomis-yaml-usability.el --- nomis yaml usability hacks ---  -*- lexical-binding: t -*-


;;;; TODO YAML
;;;; - Google for:
;;;;   emacs "yaml" hide blocks
;;;; - See https://blog.chmouel.com/2016/09/07/dealing-with-yaml-in-emacs/
;;;; - See https://stackoverflow.com/questions/1587972/how-to-display-indentation-guides-in-emacs/4459159#4459159


;; From https://stackoverflow.com/questions/1587972/how-to-display-indentation-guides-in-emacs/4459159#4459159
(defun aj-toggle-fold ()
  "Toggle fold all lines larger than indentation on current line"
  (interactive)
  (let ((col 1))
    (save-excursion
      (back-to-indentation)
      (setq col (+ 1 (current-column)))
      (set-selective-display
       (if selective-display nil (or col 1))))))



(provide 'nomis-yaml-usability)
