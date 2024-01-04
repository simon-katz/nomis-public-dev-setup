;;;; nomis-applescript.el --- nomis applescript-mode tailoring ---  -*- lexical-binding: t -*-

(require 'applescript-mode)
(require 'nomis-highlight-indentation)

(defun nomis/setup-applescript-mode ()
  ;; (nomis/wwo/set-binary-encoding-2)
  ;; (nomis/set-80-column-stuff-0)
  )

(add-hook 'applescript-mode-hook 'nomis/setup-applescript-mode)
;; (add-hook 'applescript-mode-hook 'highlight-indentation-mode)
(add-hook 'applescript-mode-hook 'highlight-indentation-current-column-mode)
;; (add-hook 'applescript-mode-hook 'yafolding-mode)
(add-hook 'applescript-mode-hook 'nomis/idle-highlight-mode)

(provide 'nomis-applescript)
