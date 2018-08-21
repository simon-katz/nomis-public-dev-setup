;;;; nomis-dirtree.el --- nomis yaml-mode tailoring ---  -*- lexical-binding: t -*-

(require 'yaml-mode)
(require 'nomis-highlight-indentation)

(defun nomis/setup-yaml-mode ()
  ;; (define-key yaml-mode-map "\C-m" 'newline-and-indent)
  )

(add-hook 'yaml-mode-hook 'nomis/setup-yaml-mode)
(add-hook 'yaml-mode-hook 'highlight-indentation-mode)
(add-hook 'yaml-mode-hook 'highlight-indentation-current-column-mode)
(add-hook 'yaml-mode-hook 'yafolding-mode)

;;;; TODO YAML -- Grab more from these places:
;;;; - Google for:
;;;;   emacs "yaml" hide blocks
;;;; - See https://blog.chmouel.com/2016/09/07/dealing-with-yaml-in-emacs/
;;;; - See https://stackoverflow.com/questions/1587972/how-to-display-indentation-guides-in-emacs/4459159#4459159
;;;;
;;;; - Make `nomis/idle-highlight-thing` work for yaml.

(provide 'nomis-yaml)
