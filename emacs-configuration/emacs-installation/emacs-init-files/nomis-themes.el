;;; nomis-themes.el --- Themes stuff -*- lexical-binding: t -*-

;;;; ___________________________________________________________________________

(require 'nomis-window-backgrounds)

;;;; ___________________________________________________________________________

(setq custom-theme-directory
      "/Users/simonkatz/development-100/repositories/nomis/dev-setup/nomis-public-dev-setup/emacs-configuration/emacs-installation/emacs-init-files/custom-themes")

;;;; ___________________________________________________________________________
;;;; ---- nomis/themes/disable-and-set-custom-themes ----

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
  (mapc #'(lambda (theme) (load-theme theme t)) (reverse themes)))

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
               (nomis/themes/current-themes-as-string)))))

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

(defconst nomis/themes/standard-light              '())
(defconst nomis/themes/standard-light+altbg1       '(nomis-alternative-background-001))
(defconst nomis/themes/standard-light+altbg2       '(nomis-alternative-background-002))
(defconst nomis/themes/standard-light+nomis        '(                                 nomis-extras-standard-light))
(defconst nomis/themes/standard-light+nomis+altbg1 '(nomis-alternative-background-001 nomis-extras-standard-light))
(defconst nomis/themes/standard-light+nomis+altbg2 '(nomis-alternative-background-002 nomis-extras-standard-light))
(defconst nomis/themes/deeper-blue                 '(                                                             deeper-blue))
(defconst nomis/themes/deeper-blue+altbg1          '(nomis-alternative-background-001                             deeper-blue))
(defconst nomis/themes/deeper-blue+altbg2          '(nomis-alternative-background-002                             deeper-blue))
(defconst nomis/themes/deeper-blue+nomis           '(                                 nomis-extras-deeper-blue    deeper-blue))
(defconst nomis/themes/deeper-blue+nomis+altbg1    '(nomis-alternative-background-001 nomis-extras-deeper-blue    deeper-blue))
(defconst nomis/themes/deeper-blue+nomis+altbg2    '(nomis-alternative-background-002 nomis-extras-deeper-blue    deeper-blue))
(defconst nomis/themes/zenburn                     '(                                                             zenburn))
(defconst nomis/themes/zenburn+altbg1              '(nomis-alternative-background-001                             zenburn))
(defconst nomis/themes/zenburn+altbg2              '(nomis-alternative-background-002                             zenburn))
(defconst nomis/themes/zenburn+nomis               '(                                 nomis-extras-zenburn        zenburn))
(defconst nomis/themes/zenburn+nomis+altbg1        '(nomis-alternative-background-001 nomis-extras-zenburn        zenburn))
(defconst nomis/themes/zenburn+nomis+altbg2        '(nomis-alternative-background-002 nomis-extras-zenburn        zenburn))
(defconst nomis/themes/dark-laptop                 '(                                                             dark-laptop))
(defconst nomis/themes/dark-laptop+nomis           '(                                 nomis-extras-dark-laptop    dark-laptop))

(defconst nomis/themes/pairs
  `(("Standard-Light + Nomis         " ,nomis/themes/standard-light+nomis)
    ("Dark Laptop    + Nomis         " ,nomis/themes/dark-laptop+nomis)
    ("Deeper-Blue    + Nomis         " ,nomis/themes/deeper-blue+nomis)
    ("Zenburn        + Nomis         " ,nomis/themes/zenburn+nomis)
    ("______                         " ())
    ("Standard-Light + Nomis + AltBg1" ,nomis/themes/standard-light+nomis+altbg1)
    ("Deeper-Blue    + Nomis + AltBg1" ,nomis/themes/deeper-blue+nomis+altbg1)
    ("Zenburn        + Nomis + AltBg1" ,nomis/themes/zenburn+nomis+altbg1)
    ("______                         " ())
    ("Standard-Light + Nomis + AltBg2" ,nomis/themes/standard-light+nomis+altbg2)
    ("Deeper-Blue    + Nomis + AltBg2" ,nomis/themes/deeper-blue+nomis+altbg2)
    ("Zenburn        + Nomis + AltBg2" ,nomis/themes/zenburn+nomis+altbg2)
    ("______                         " ())
    ("Standard-Light                 " ,nomis/themes/standard-light)
    ("Dark Laptop                    " ,nomis/themes/dark-laptop)
    ("Deeper-Blue                    " ,nomis/themes/deeper-blue)
    ("Zenburn                        " ,nomis/themes/zenburn)
    ("______                         " ())
    ("Standard-Light         + AltBg1" ,nomis/themes/standard-light+altbg1)
    ("Deeper-Blue            + AltBg1" ,nomis/themes/deeper-blue+altbg1)
    ("Zenburn                + AltBg1" ,nomis/themes/zenburn+altbg1)
    ("______                         " ())
    ("Standard-Light         + AltBg2" ,nomis/themes/standard-light+altbg2)
    ("Deeper-Blue            + AltBg2" ,nomis/themes/deeper-blue+altbg2)
    ("Zenburn                + AltBg2" ,nomis/themes/zenburn+altbg2)
    ("===============================" ())))

(defun -nomis/themes/themes->string (themes)
  (->> (rassoc (list themes)
               nomis/themes/pairs)
       car
       s-trim))

(defun nomis/themes/current-themes-as-string ()
  (-nomis/themes/themes->string custom-enabled-themes))

(defun nomis/themes/set-custom-themes ()
  (interactive)
  (let* ((prompt (format "Choose themes — currently %S: "
                         (nomis/themes/current-themes-as-string)))
         (response (ido-completing-read prompt
                                        nomis/themes/pairs
                                        nil
                                        t
                                        nil
                                        'nomis/themes/set-custom-themes/prompt-history))
         (chosen-themes (cadr (assoc response nomis/themes/pairs))))
    (nomis/themes/disable-and-set-custom-themes chosen-themes)))

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
                           (-map #'-nomis/themes/themes->string)
                           (-map (lambda (s)
                                   (s-concat "                "
                                             s)))))))))
    (message "________________________________")
    (message "\n-nomis/themes/history/prevs:\n%s"
             (funcall themes*->string -nomis/themes/history/prevs))
    (message "\n-nomis/themes/history/nexts:\n%s\n"
             (funcall themes*->string -nomis/themes/history/nexts)))
  (message "Current themes: %s\n(See *Messages* buffer for history list)"
           (nomis/themes/current-themes-as-string)))

;;;; ___________________________________________________________________________

;; (defun nomis/dark-background-mode? ()
;;   (eq (frame-parameter nil 'background-mode) 'dark))

;;;; ___________________________________________________________________________

(provide 'nomis-themes)
