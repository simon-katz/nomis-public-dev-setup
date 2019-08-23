;; Init stuff -- ido and smex

;;;; ___________________________________________________________________________
;;;; ____ * ido

;;;; See https://www.emacswiki.org/emacs/InteractivelyDoThings

;; TODO: Look at this ido stuff copied from the Starter Kit.

(ido-mode t)
(ido-everywhere 1)

(require 'ido-completing-read+)
(ido-ubiquitous-mode 1) ; e.g. for `describe-function' (C-h f)

(setq ido-default-file-method 'selected-window)
(setq ido-default-buffer-method 'selected-window)

;; (setq ido-enable-prefix nil)
(setq ido-enable-flex-matching t)
;; (setq ido-auto-merge-work-directories-length nil)
;; (setq ido-create-new-buffer 'always)
(setq ido-use-filename-at-point 'guess)
;; (setq ido-use-virtual-buffers t)
;; (setq ido-handle-duplicate-virtual-buffers 2)
;; (setq ido-max-prospects 10)

(progn ; see https://github.com/creichert/ido-vertical-mode.el
  (ido-vertical-mode 1)
  (setq ido-vertical-show-count t)
  (setq ido-vertical-define-keys 'C-n-C-p-up-down-left-right)
  (setq ido-use-faces t)
  (set-face-attribute 'ido-vertical-first-match-face nil
                      :background nil
                      :foreground "blue")
  (set-face-attribute 'ido-vertical-only-match-face nil
                      :background nil
                      :foreground nil)
  (set-face-attribute 'ido-vertical-match-face nil
                      :foreground nil))

;;;; ___________________________________________________________________________
;;;; ____ * smex normal stuff

;;;; See https://www.emacswiki.org/emacs/Smex

;;;; See https://github.com/nonsequitur/smex/
;;;;   Smex is a M-x enhancement for Emacs. Built on top of Ido, it provides a
;;;;   convenient interface to your recently and most frequently used commands.
;;;;   And to all the other commands, too.

;;;; Point of info:
;;;;   One reason why smex is better than execute-extended-command is that
;;;;   smex remembers the last command, whereas execute-extended-command
;;;;   (even with ido-ubiquitous) does not.

(smex-initialize) ; Can be omitted. This might cause a (minimal) delay
                  ; when Smex is auto-initialized on its first run.

(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;;;; ___________________________________________________________________________
;;;; ____ * smex -- Hack to allow overcoming of smex breaking `last-command`

;;;;`last-command` doesn't work with smex. Sheesh!
;;;; Here's a hack to allow that to be overcome.
;;;; Use:
;;;;     `(or (bound-and-true-p *nomis/smex/last-command*) last-command)`
;;;;
;;;; TODO Maybe also need to make this work with `smex-major-mode-commands`,
;;;;      which you've never used.

(defvar *nomis/smex/last-command* nil)

(advice-add 'smex
            :around
            (lambda (orig-fun &rest args)
              (let* ((*nomis/smex/last-command* last-command))
                (apply orig-fun args)))
            '((name . allow-overcoming-of-smex-blatting-last-command)))

;;;; ___________________________________________________________________________
;;;; ____ * End

(provide 'nomis-ido-and-smex)
