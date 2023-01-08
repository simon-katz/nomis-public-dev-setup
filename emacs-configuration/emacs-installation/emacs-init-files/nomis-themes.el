;;; nomis-themes.el --- Themes stuff -*- lexical-binding: t -*-

;;;; ___________________________________________________________________________

(require 'nomis-window-backgrounds)

;;;; ___________________________________________________________________________

(setq custom-theme-directory
      "/Users/simonkatz/development-100/repositories/nomis/dev-setup/nomis-public-dev-setup/emacs-configuration/emacs-installation/emacs-init-files/custom-themes")

;;;; ___________________________________________________________________________
;;;; ---- nomis/themes/disable-and-set-custom-themes ----

(defun nomis/themes/disable-and-set-custom-themes (themes)
  "Disable all custom themes and then enable the supplied themes."
  ;; Disabling and (re-)enabling is a fix for custom themes not being set
  ;; up properly in some situations.
  ;; - When disabling and enabling themes.
  ;; - When creating new frames.
  ;;   - /eg/ Incorrect `hl-line` colours when you have custom dark themes.
  ;;   - /eg/ `M-x` in the new frame giving wrong frame background colours
  ;;     (which might be OK now).
  ;; Maybe this is an alternative. You had this somewhere once.
  ;; doesn't fix things. Grrrr! This fixes things:
  ;; (let* ((current-f (selected-frame)))
  ;;   (dolist (f (frame-list))
  ;;     (select-frame f)
  ;;     (let* ((current-b (current-buffer)))
  ;;       (dolist (b (buffer-list))
  ;;         (when (get-buffer-window-list b)
  ;;           (switch-to-buffer b)))
  ;;       (switch-to-buffer current-b)))
  ;;   (select-frame current-f))
  (mapc #'disable-theme custom-enabled-themes)
  (mapc #'(lambda (theme) (load-theme theme t)) (reverse themes)))

;;;; ___________________________________________________________________________
;;;; ---- Fix problem with bad themes in new frames ----

(defun -nomis/themes/disable-and-reload-custom-themes (_frame)
  (nomis/themes/disable-and-set-custom-themes custom-enabled-themes))

(add-hook 'after-make-frame-functions
          '-nomis/themes/disable-and-reload-custom-themes
          100)

;;;; ___________________________________________________________________________
;;;; ---- nomis/themes/set-custom-themes ----

(defconst nomis/themes/standard-light              '())
(defconst nomis/themes/standard-light+altbg1       '(nomis-alternative-background-001))
(defconst nomis/themes/standard-light+altbg2       '(nomis-alternative-background-002))
(defconst nomis/themes/standard-light+nomis        '(                                 nomis-extras-standard-light))
(defconst nomis/themes/standard-light+nomis+altbg1 '(nomis-alternative-background-001 nomis-extras-standard-light))
(defconst nomis/themes/standard-light+nomis+altbg2 '(nomis-alternative-background-002 nomis-extras-standard-light))
(defconst nomis/themes/deeper-blue                 '(                                 deeper-blue))
(defconst nomis/themes/deeper-blue+altbg1          '(nomis-alternative-background-001 deeper-blue))
(defconst nomis/themes/deeper-blue+altbg2          '(nomis-alternative-background-002 deeper-blue))
(defconst nomis/themes/deeper-blue+nomis           '(                                 nomis-extras-deeper-blue deeper-blue))
(defconst nomis/themes/deeper-blue+nomis+altbg1    '(nomis-alternative-background-001 nomis-extras-deeper-blue deeper-blue))
(defconst nomis/themes/deeper-blue+nomis+altbg2    '(nomis-alternative-background-002 nomis-extras-deeper-blue deeper-blue))
(defconst nomis/themes/zenburn                     '(                                 zenburn))
(defconst nomis/themes/zenburn+altbg1              '(nomis-alternative-background-001 zenburn))
(defconst nomis/themes/zenburn+altbg2              '(nomis-alternative-background-002 zenburn))

(defun nomis/themes/set-custom-themes ()
  (interactive)
  (let* ((completions
          `(("Standard-Light                 " ,nomis/themes/standard-light)
            ("Standard-Light         + AltBg1" ,nomis/themes/standard-light+altbg1)
            ("Standard-Light         + AltBg2" ,nomis/themes/standard-light+altbg2)
            ("______                         " ())
            ("Standard-Light + Nomis         " ,nomis/themes/standard-light+nomis)
            ("Standard-Light + Nomis + AltBg1" ,nomis/themes/standard-light+nomis+altbg1)
            ("Standard-Light + Nomis + AltBg2" ,nomis/themes/standard-light+nomis+altbg2)
            ("______                         " ())
            ("Deeper-Blue                    " ,nomis/themes/deeper-blue)
            ("Deeper-Blue            + AltBg1" ,nomis/themes/deeper-blue+altbg1)
            ("Deeper-Blue            + AltBg2" ,nomis/themes/deeper-blue+altbg2)
            ("______                         " ())
            ("Deeper-Blue + Nomis            " ,nomis/themes/deeper-blue+nomis)
            ("Deeper-Blue + Nomis    + AltBg1" ,nomis/themes/deeper-blue+nomis+altbg1)
            ("Deeper-Blue + Nomis    + AltBg2" ,nomis/themes/deeper-blue+nomis+altbg2)
            ("______                         " ())
            ("Zenburn                        " ,nomis/themes/zenburn)
            ("Zenburn                + AltBg1" ,nomis/themes/zenburn+altbg1)
            ("Zenburn                + AltBg2" ,nomis/themes/zenburn+altbg2)
            ("===============================" ())))
         (prompt (format "Choose themes — currently %S: "
                         (->> (rassoc (list custom-enabled-themes)
                                      completions)
                              car
                              s-trim)))
         (response (ido-completing-read prompt
                                        completions
                                        nil
                                        t
                                        nil
                                        'nomis/themes/set-custom-themes/prompt-history))
         (chosen-themes (cadr (assoc response completions))))
    (nomis/themes/disable-and-set-custom-themes chosen-themes)))

;;;; ___________________________________________________________________________

;; (defun nomis/dark-background-mode? ()
;;   (eq (frame-parameter nil 'background-mode) 'dark))

;;;; ___________________________________________________________________________

(provide 'nomis-themes)
