;;; nomis-themes.el --- Themes stuff -*- lexical-binding: t -*-

;;;; ___________________________________________________________________________

(require 'nomis-window-backgrounds)

;;;; ___________________________________________________________________________

(setq custom-theme-directory
      "/Users/simonkatz/development-100/repositories/nomis/dev-setup/nomis-public-dev-setup/emacs-configuration/emacs-installation/emacs-init-files/custom-themes")

;;;; ___________________________________________________________________________
;;;; Fix for custom themes not being applied properly when creating new frames.
;;;; - /eg/ Incorrect `hl-line` colours when you have custom dark themes.
;;;; - /eg/ `M-x` in the new frame giving wrong frame background colours.

(defun -nomis/themes/disable-and-reload-custom-themes (_frame)
  (nomis/window-backgrounds/disable-and-set-custom-themes custom-enabled-themes))

(add-hook 'after-make-frame-functions
          '-nomis/themes/disable-and-reload-custom-themes
          100)

;;;; ___________________________________________________________________________

(defun nomis/themes/set-custom-themes ()
  (interactive)
  (let* ((prompt (format "Choose themes — currently %S: "
                         custom-enabled-themes))
         (completions
          '(("Nomis Standard Light" (nomis-extras-standard-light))
            ("Nomis deeper-blue"    (nomis-extras-deeper-blue deeper-blue))
            ("Standard Light"       nil)
            ("Raw deeper-blue"      (deeper-blue))))
         (response (ido-completing-read prompt
                                        completions
                                        nil
                                        t))
         (chosen-themes (cadr (assoc response completions)))
         (existing-background-themes (intersection
                                      custom-enabled-themes
                                      nomis/window-backgrounds/themes))
         (new-themes (-concat existing-background-themes
                              chosen-themes)))
    (nomis/window-backgrounds/disable-and-set-custom-themes new-themes)))

;;;; ___________________________________________________________________________

;; (defun nomis/dark-background-mode? ()
;;   (eq (frame-parameter nil 'background-mode) 'dark))

;;;; ___________________________________________________________________________

(provide 'nomis-themes)
