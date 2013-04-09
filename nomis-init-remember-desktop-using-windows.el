;;;; Init stuff -- Remembering desktop using windows.

;;;; ___________________________________________________________________________
;;;; ---- saveplace ----

;;;; I'm not sure, but this may be need to be done before the windows
;;;; stuff below in order for things under the control of windows to
;;;; work.

(require 'saveplace)
(setq-default save-place t)


;;;; ___________________________________________________________________________
;;;; ---- windows ----

(require 'windows)

(win:startup-with-window)
(add-hook 'window-setup-hook 'resume-windows)
(setq win:no-raise-at-save t)

(setq win:local-config-file
      "~/Documents/jsk/development-100/__for-sync/jsk-settings/emacs-window-configurations/")

(progn
  ;; C-x C-c by default does save-buffers-kill-terminal.
  ;; We want...
  (define-key ctl-x-map (kbd "C-c") 'see-you-again)
  (define-key ctl-x-map (kbd "c") 'save-buffers-kill-terminal))

;; Store configurations in this dir...
(;; Instead of this, which I can't make work, I've hacked the
 ;; "windows.el" source file.
 ;; (setq win:switch-prefix "\C-c\C-q") ; (setq win:switch-prefix "\C-cw")
 ;; (define-key global-map win:switch-prefix nil)
 ;; (define-key global-map "\C-cw1" 'win-switch-to-window)
 )
