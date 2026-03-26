;;; nomis-themes.el --- Themes stuff -*- lexical-binding: t -*-

;;;; ___________________________________________________________________________

(require 'nomis-window-backgrounds) ; noflycheck

;;;; ___________________________________________________________________________

(setq custom-theme-directory
      "/Users/simonkatz/development-100/repositories/nomis/dev-setup/nomis-public-dev-setup/emacs-configuration/emacs-installation/emacs-init-files/custom-themes")

;;;; ___________________________________________________________________________
;;;; ---- nomis/themes/disable-and-set-custom-themes ----

(defvar nomis/themes/theme-changed-hook ())

(defun nomis/themes/disable-and-set-custom-themes* (themes)
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
  (mapc #'(lambda (theme) (load-theme theme t)) (reverse themes))
  (run-hooks 'nomis/themes/theme-changed-hook))

(defun nomis/themes/disable-and-set-custom-themes (themes)
  "Disable all custom themes and then enable the supplied themes."
  ;; Disabling and (re-)enabling is a fix for custom themes not being set
  ;; up properly in some situations.
  ;; - When disabling and enabling themes.
  ;; - When creating new frames.
  ;;   - /eg/ Incorrect `hl-line` colours when you have custom dark themes.
  ;;   - /eg/ `M-x` in the new frame giving wrong frame background colours
  ;;     (which might be OK now).
  ;; It's also useful so we have a place to record theme history. :-)
  (unless (equal themes custom-enabled-themes)
    (-nomis/themes/history/note-current-themes))
  (nomis/themes/disable-and-set-custom-themes* themes))

;;;; ___________________________________________________________________________
;;;; ---- Fix problem with bad themes in new frames ----

(defun -nomis/themes/disable-and-reload-custom-themes (_frame)
  (nomis/themes/disable-and-set-custom-themes* custom-enabled-themes))

(add-hook 'after-make-frame-functions
          '-nomis/themes/disable-and-reload-custom-themes
          100)

;;;; ___________________________________________________________________________
;;;; ---- History ----

(defvar -nomis/themes/history/prevs '())
(defvar -nomis/themes/history/nexts '())

(defun -nomis/themes/history/note-current-themes ()
  (push custom-enabled-themes -nomis/themes/history/prevs))

(defun -nomis/themes/history/prev-next-helper (sym-1 sym-2)
  (if (and (null (symbol-value sym-1))
           (null (symbol-value sym-2)))
      (progn
        (nomis/msg/grab-user-attention/high)
        (error "Theme history is empty"))
    (let* ((wrapped? nil))
      (if (null (symbol-value sym-1))
          (let* ((nexts (symbol-value sym-2)))
            (setq wrapped? t)
            (setf (symbol-value sym-1) (-concat (reverse (butlast nexts))
                                                (list custom-enabled-themes)))
            (setf (symbol-value sym-2) nil)
            (nomis/themes/disable-and-set-custom-themes* (first (last nexts))))
        (progn
          (push custom-enabled-themes (symbol-value sym-2))
          (let* ((themes (pop (symbol-value sym-1))))
            (nomis/themes/disable-and-set-custom-themes* themes))))
      (when wrapped?
        (nomis/msg/grab-user-attention/low))
      (message "%sThemes are now: %s"
               (if wrapped? "(Wrapped) " "")
               (-nomis/themes/current-themes-text)))))

(defun nomis/themes/history/prev ()
  (interactive)
  (-nomis/themes/history/prev-next-helper '-nomis/themes/history/prevs
                                          '-nomis/themes/history/nexts))

(defun nomis/themes/history/next ()
  (interactive)
  (-nomis/themes/history/prev-next-helper '-nomis/themes/history/nexts
                                          '-nomis/themes/history/prevs))

(defun nomis/themes/history/clear ()
  (interactive)
  (setq -nomis/themes/history/prevs nil)
  (setq -nomis/themes/history/nexts nil))

;;;; ___________________________________________________________________________
;;;; ---- nomis/themes/set-custom-themes ----

(defconst nomis/themes/standard-light+nomis        '(                                 nomis-extras-standard-light nomis-common-light))
(defconst nomis/themes/dark-laptop+nomis           '(                                 nomis-extras-dark-laptop    nomis-common-dark  dark-laptop))
(defconst nomis/themes/deeper-blue+nomis           '(                                 nomis-extras-deeper-blue    nomis-common-dark  deeper-blue))
(defconst nomis/themes/zenburn+nomis               '(                                 nomis-extras-zenburn        nomis-common-dark  zenburn))
(defconst nomis/themes/standard-light+nomis+altbg1 '(nomis-alternative-background-001 nomis-extras-standard-light nomis-common-light))
(defconst nomis/themes/deeper-blue+nomis+altbg1    '(nomis-alternative-background-001 nomis-extras-deeper-blue    nomis-common-dark  deeper-blue))
(defconst nomis/themes/zenburn+nomis+altbg1        '(nomis-alternative-background-001 nomis-extras-zenburn        nomis-common-dark  zenburn))
(defconst nomis/themes/standard-light+nomis+altbg2 '(nomis-alternative-background-002 nomis-extras-standard-light nomis-common-light))
(defconst nomis/themes/deeper-blue+nomis+altbg2    '(nomis-alternative-background-002 nomis-extras-deeper-blue    nomis-common-dark  deeper-blue))
(defconst nomis/themes/zenburn+nomis+altbg2        '(nomis-alternative-background-002 nomis-extras-zenburn        nomis-common-dark  zenburn))
(defconst nomis/themes/standard-light              '())
(defconst nomis/themes/dark-laptop                 '(                                                                                dark-laptop))
(defconst nomis/themes/deeper-blue                 '(                                                                                deeper-blue))
(defconst nomis/themes/zenburn                     '(                                                                                zenburn))
(defconst nomis/themes/standard-light+altbg1       '(nomis-alternative-background-001                             nomis-common-light))
(defconst nomis/themes/deeper-blue+altbg1          '(nomis-alternative-background-001                             nomis-common-dark  deeper-blue))
(defconst nomis/themes/zenburn+altbg1              '(nomis-alternative-background-001                             nomis-common-dark  zenburn))
(defconst nomis/themes/standard-light+altbg2       '(nomis-alternative-background-002                             nomis-common-light))
(defconst nomis/themes/deeper-blue+altbg2          '(nomis-alternative-background-002                             nomis-common-dark  deeper-blue))
(defconst nomis/themes/zenburn+altbg2              '(nomis-alternative-background-002                             nomis-common-dark  zenburn))

(defconst -nomis/themes/pairs
  `((,nomis/themes/standard-light+nomis        . "Standard-Light + Nomis         ")
    (,nomis/themes/dark-laptop+nomis           . "Dark Laptop    + Nomis         ")
    (,nomis/themes/deeper-blue+nomis           . "Deeper-Blue    + Nomis         ")
    (,nomis/themes/zenburn+nomis               . "Zenburn        + Nomis         ")
    (()                                        . "______                         ")
    (,nomis/themes/standard-light+nomis+altbg1 . "Standard-Light + Nomis + AltBg1")
    (,nomis/themes/deeper-blue+nomis+altbg1    . "Deeper-Blue    + Nomis + AltBg1")
    (,nomis/themes/zenburn+nomis+altbg1        . "Zenburn        + Nomis + AltBg1")
    (()                                        . "______                         ")
    (,nomis/themes/standard-light+nomis+altbg2 . "Standard-Light + Nomis + AltBg2")
    (,nomis/themes/deeper-blue+nomis+altbg2    . "Deeper-Blue    + Nomis + AltBg2")
    (,nomis/themes/zenburn+nomis+altbg2        . "Zenburn        + Nomis + AltBg2")
    (()                                        . "______                         ")
    (,nomis/themes/standard-light              . "Standard-Light                 ")
    (,nomis/themes/dark-laptop                 . "Dark Laptop                    ")
    (,nomis/themes/deeper-blue                 . "Deeper-Blue                    ")
    (,nomis/themes/zenburn                     . "Zenburn                        ")
    (()                                        . "______                         ")
    (,nomis/themes/standard-light+altbg1       . "Standard-Light         + AltBg1")
    (,nomis/themes/deeper-blue+altbg1          . "Deeper-Blue            + AltBg1")
    (,nomis/themes/zenburn+altbg1              . "Zenburn                + AltBg1")
    (()                                        . "______                         ")
    (,nomis/themes/standard-light+altbg2       . "Standard-Light         + AltBg2")
    (,nomis/themes/deeper-blue+altbg2          . "Deeper-Blue            + AltBg2")
    (,nomis/themes/zenburn+altbg2              . "Zenburn                + AltBg2")
    (()                                        . "===============================")))

(defun -nomis/themes/themes-text (input-themes)
  (cl-loop for (themes . text) in -nomis/themes/pairs
           when (equal themes input-themes)
           return text
           finally (return "Unexpected value for custom-enabled-themes")))

(defun -nomis/themes/current-themes-text ()
  (-nomis/themes/themes-text custom-enabled-themes))

(defun nomis/themes/set-custom-themes ()
  (interactive)
  (let* ((curr-string (-nomis/themes/current-themes-text))
         (prompt (format "Choose themes — currently %S: "
                         curr-string))
         (response (ido-completing-read prompt
                                        (-map #'cdr -nomis/themes/pairs)
                                        nil
                                        t
                                        nil
                                        'nomis/themes/set-custom-themes/prompt-history
                                        curr-string))
         (v (car (rassoc response
                         -nomis/themes/pairs))))
    (nomis/themes/disable-and-set-custom-themes v)))

;;;; ___________________________________________________________________________

(defun nomis/themes/report-custom-themes ()
  (interactive)
  (let* ((inhibit-message t)
         (themes*->string
          (lambda (themes*)
            (if (null themes*)
                "                <empty>"
              (s-join "\n"
                      (->> themes*
                           (-map #'-nomis/themes/themes-text)
                           (-map (lambda (s)
                                   (s-concat "                "
                                             s)))))))))
    (message "________________________________")
    (message "\n-nomis/themes/history/prevs:\n%s"
             (funcall themes*->string -nomis/themes/history/prevs))
    (message "\n-nomis/themes/history/nexts:\n%s\n"
             (funcall themes*->string -nomis/themes/history/nexts)))
  (message "Current themes: %s\n(See *Messages* buffer for history list)"
           (-nomis/themes/current-themes-text)))

;;;; ___________________________________________________________________________

(defun nomis/dark-background-mode? ()
  (eq (frame-parameter nil 'background-mode) 'dark))

;;;; ___________________________________________________________________________

(provide 'nomis-themes)
