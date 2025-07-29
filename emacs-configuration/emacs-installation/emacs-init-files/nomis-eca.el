;;;; Init stuff -- nomis-eca --  -*- lexical-binding: t -*-

;;;; ___________________________________________________________________________

(require 'eca)

(keymap-unset eca-chat-mode-map "C-k") ; was `eca-chat-reset`

;; (setq eca-custom-command '("/Users/simonkatz/bin-private/eca" "server" "--log-level" "debug"))

;;;; ___________________________________________________________________________

(provide 'nomis-eca)
