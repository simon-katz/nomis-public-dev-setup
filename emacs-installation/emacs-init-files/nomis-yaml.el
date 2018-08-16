;;;; nomis-dirtree.el --- nomis yaml-mode tailoring ---  -*- lexical-binding: t -*-

(require 'yaml-mode)

(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))

(add-hook 'yaml-mode-hook
          '(lambda ()
             (define-key yaml-mode-map "\C-m" 'newline-and-indent)))

(provide 'nomis-yaml)
