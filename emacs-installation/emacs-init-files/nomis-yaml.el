;;;; nomis-dirtree.el --- nomis yaml-mode tailoring ---  -*- lexical-binding: t -*-

(require 'yaml-mode)
(require 'nomis-highlight-indentation)

(defun nomis/setup-yaml-mode ()
  ;; (define-key yaml-mode-map "\C-m" 'newline-and-indent)
  (highlight-indentation-mode)
  (highlight-indentation-current-column-mode))

(add-hook 'yaml-mode-hook 'nomis/setup-yaml-mode)


;;;; TODO YAML -- Grab more from these places:
;;;; - Google for:
;;;;   emacs "yaml" hide blocks
;;;; - See https://blog.chmouel.com/2016/09/07/dealing-with-yaml-in-emacs/
;;;; - See https://stackoverflow.com/questions/1587972/how-to-display-indentation-guides-in-emacs/4459159#4459159

(provide 'nomis-yaml)
