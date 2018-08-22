;;;; nomis-dirtree.el --- nomis yaml-mode tailoring ---  -*- lexical-binding: t -*-

(require 'yaml-mode)
(require 'nomis-highlight-indentation)

(defun nomis/setup-yaml-mode ()
  ;; (define-key yaml-mode-map "\C-m" 'newline-and-indent)
  (nomis/set-80-column-stuff-0))

(add-hook 'yaml-mode-hook 'nomis/setup-yaml-mode)
(add-hook 'yaml-mode-hook 'highlight-indentation-mode)
(add-hook 'yaml-mode-hook 'highlight-indentation-current-column-mode)
(add-hook 'yaml-mode-hook 'yafolding-mode)
(add-hook 'yaml-mode-hook 'nomis/idle-highlight-mode)

;;;; TODO YAML -- Grab more from these places:
;;;; - Maybe look again at https://blog.chmouel.com/2016/09/07/dealing-with-yaml-in-emacs/
;;;; - Make `nomis/idle-highlight-thing` work for yaml.

(provide 'nomis-yaml)
