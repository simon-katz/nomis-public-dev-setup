;;; nomis-vterm.el --- vterm tailoring -*- lexical-binding: t; -*-

;;;; ___________________________________________________________________________

(with-eval-after-load 'vterm

  (setq vterm-max-scrollback 10000)
  (setq vterm-clear-scrollback-when-clearing t)

  (define-key vterm-mode-map (kbd "M-k") 'vterm-clear))

;;;; ___________________________________________________________________________

(provide 'nomis-vterm)
