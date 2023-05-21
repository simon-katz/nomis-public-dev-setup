;;; nomis-vterm.el --- vterm tailoring -*- lexical-binding: t; -*-

;;;; ___________________________________________________________________________

;;;; Emacs libvterm README says
;;;;   And add this to your init.el:
;;;;     (add-to-list 'load-path "path/to/emacs-libvterm")
;;;;     (require 'vterm)

(add-to-list 'load-path "/Users/simonkatz/development-100/repositories/software-installations/emacs-libvterm")

;; (require 'vterm) ; Not needed. (But see abovedocumentation.)

(with-eval-after-load 'vterm

  (setq vterm-max-scrollback 100000)
  (setq vterm-clear-scrollback-when-clearing t)
  (setq vterm-copy-mode-remove-fake-newlines t)

  (define-key vterm-mode-map (kbd "M-k") 'vterm-clear))

;;;; ___________________________________________________________________________

(provide 'nomis-vterm)
