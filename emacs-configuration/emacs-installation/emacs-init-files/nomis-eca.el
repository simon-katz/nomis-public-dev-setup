;;;; Init stuff -- nomis-eca --  -*- lexical-binding: t -*-

;;;; ___________________________________________________________________________

(require 'eca)

;; TODO: Create a PR for this:
;; See
;; (defvar eca-chat-mode-map
;;   (let ((map (make-sparse-keymap)))
;;     (define-key map (kbd "<backspace>") #'eca-chat--key-pressed-backspace)
;;     (define-key map (kbd "DEL") #'eca-chat--key-pressed-backspace)
;;     (define-key map (kbd "S-<return>") #'eca-chat--key-pressed-newline)
;;     (define-key map (kbd "C-<up>") #'eca-chat--key-pressed-previous-prompt-history)
;;     (define-key map (kbd "C-<down>") #'eca-chat--key-pressed-next-prompt-history)
;;     (define-key map (kbd "C-k") #'eca-chat-reset)
;;     (define-key map (kbd "C-l") #'eca-chat-clear)
;;     (define-key map (kbd "C-t") #'eca-chat-talk)
;;     (define-key map (kbd "<return>") #'eca-chat--key-pressed-return)
;;     (define-key map (kbd "<tab>") #'eca-chat--key-pressed-tab)
;;     map)
;;   "Keymap used by `eca-chat-mode'.")
(keymap-unset eca-chat-mode-map "C-k")
(keymap-unset eca-chat-mode-map "C-l")
(keymap-unset eca-chat-mode-map "C-t")
(define-key eca-chat-mode-map (kbd "C-c C-k") #'eca-chat-reset)
(define-key eca-chat-mode-map (kbd "C-c C-l") #'eca-chat-clear)
(define-key eca-chat-mode-map (kbd "C-c C-t") #'eca-chat-talk)

;; The following is just for me, not the PR.

(keymap-set eca-chat-mode-map "H-k" #'eca-chat-clear)

(keymap-set eca-chat-mode-map "M-p" #'eca-chat--key-pressed-previous-prompt-history)
(keymap-set eca-chat-mode-map "M-n" #'eca-chat--key-pressed-next-prompt-history)

;; (setq eca-custom-command '("/Users/simonkatz/bin-private/eca" "server" "--log-level" "debug"))

;;;; ___________________________________________________________________________

(provide 'nomis-eca)
