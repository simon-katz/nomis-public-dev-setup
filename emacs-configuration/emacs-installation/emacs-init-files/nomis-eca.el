;;; Init stuff -- nomis-eca --  -*- lexical-binding: t -*-

;;; Code:

;;;; Require things

(require 'eca nil t)

;;;; The rest

(when (featurep 'eca) ; eca changes rapidly, so we don't have it in git

  (setq eca-chat-use-side-window nil)

  (set-face-attribute 'eca-chat-user-messages-face nil :foreground "grey95")
  (set-face-attribute 'eca-chat-user-messages-face nil :background "#580058")

  (set-face-attribute 'eca-chat-approval-modeline-face nil :background "yellow")

  (defun nomis/setup-eca-chat-mode ()
    (nomis/wwo/mode)
    (nomis/wwo/set-binary-encoding-0))

  (add-hook 'eca-chat-mode-hook 'nomis/setup-eca-chat-mode)

  (keymap-set eca-chat-mode-map "H-k" #'eca-chat-clear)
  (keymap-set eca-chat-mode-map "M-p" #'eca-chat--key-pressed-previous-prompt-history)
  (keymap-set eca-chat-mode-map "M-n" #'eca-chat--key-pressed-next-prompt-history)

  ;; (setq eca-custom-command '("/Users/simonkatz/bin-private/eca" "server" "--log-level" "debug"))
  )

;;; End

(provide 'nomis-eca)
