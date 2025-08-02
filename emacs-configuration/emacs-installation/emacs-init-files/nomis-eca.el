;;;; Init stuff -- nomis-eca --  -*- lexical-binding: t -*-

;;;; ___________________________________________________________________________

(require 'eca)

(keymap-set eca-chat-mode-map "H-k" #'eca-chat-clear)
(keymap-set eca-chat-mode-map "M-p" #'eca-chat--key-pressed-previous-prompt-history)
(keymap-set eca-chat-mode-map "M-n" #'eca-chat--key-pressed-next-prompt-history)

;; (setq eca-custom-command '("/Users/simonkatz/bin-private/eca" "server" "--log-level" "debug"))

;;;; ___________________________________________________________________________

(provide 'nomis-eca)
