;;;; Init stuff --- JSON ---  -*- lexical-binding: t -*-

;;;; ___________________________________________________________________________

(defun nomis/set-up-json-mode ()
  (make-local-variable 'js-indent-level)
  (setq js-indent-level 2))

(add-hook 'json-mode-hook
          'nomis/set-up-json-mode)

;;;; ___________________________________________________________________________

(provide 'nomis-json)
