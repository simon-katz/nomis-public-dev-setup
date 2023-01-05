;;;; Init stuff --- Window backgrounds ---  -*- lexical-binding: t -*-

;;;; ___________________________________________________________________________
;;;; ---- nomis/window-backgrounds/disable-and-set-custom-themes ----

(defun nomis/window-backgrounds/disable-and-set-custom-themes (themes)
  "Disable all custom themes and then enable the supplied themes.
A fix for custom themes not being set up properly.
For new frames (that might be OK now) and when disabling and enabling themes."
  (interactive (list custom-enabled-themes))
  (mapc #'disable-theme custom-enabled-themes)
  (mapc #'(lambda (theme) (load-theme theme t)) (reverse themes)))

;; Maybe this is an alternatove. You had this somwhere once.
;; The following seems to no longer be needed.
;; The above doesn't update the display properly. Using `redraw-display`
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

;;;; ___________________________________________________________________________
;;;; ---- Use a different background for unselected windows ----

(defun -nomis/window-backgrounds/turn-on-auto-dim ()
  (when (fboundp 'auto-dim-other-buffers-mode)
    (auto-dim-other-buffers-mode t)))

(add-hook 'after-init-hook
          '-nomis/window-backgrounds/turn-on-auto-dim)

;;;; ___________________________________________________________________________
;;;; ---- Changing window backgrounds ----

(defconst nomis/window-backgrounds/themes
  '(nomis-alternative-background-001
    nomis-alternative-background-002))

(defconst -nomis/window-backgrounds/nil-and-themes
  (cons nil nomis/window-backgrounds/themes))

(defvar -nomis/window-backgrounds/current-index 0)

(defun -nomis/window-backgrounds/next-index ()
  (mod (1+ -nomis/window-backgrounds/current-index)
       (length -nomis/window-backgrounds/nil-and-themes)))

(defun -nomis/window-backgrounds/current-theme-or-nil ()
  (nth -nomis/window-backgrounds/current-index
       -nomis/window-backgrounds/nil-and-themes))

(defun -nomis/window-backgrounds/set (n)
  (setq -nomis/window-backgrounds/current-index n)
  (case 2
    (1
     ;; This gets into a weird state with dark stuff. What's displayed is a mix
     ;; of `(background light)` and `(background dark)` face attributes.
     ;; You get this when cycling window backgrounds with a dark theme.
     (mapc #'disable-theme nomis/window-backgrounds/themes)
     (let* ((new-theme (-nomis/window-backgrounds/current-theme-or-nil)))
       (when new-theme
         (load-theme new-theme t))))
    (2
     (let* ((themes
             (->> (set-difference custom-enabled-themes
                                  nomis/window-backgrounds/themes)
                  (-concat
                   (let* ((new-theme
                           (-nomis/window-backgrounds/current-theme-or-nil)))
                     (when new-theme
                       (list new-theme)))))))
       (nomis/window-backgrounds/disable-and-set-custom-themes themes)))))

(defun nomis/window-backgrounds/cycle ()
  (interactive)
  (-nomis/window-backgrounds/set (-nomis/window-backgrounds/next-index)))

(defun nomis/window-backgrounds/set-alternative ()
  (interactive)
  (-nomis/window-backgrounds/set 1))

(defun nomis/window-backgrounds/set-default ()
  (interactive)
  (-nomis/window-backgrounds/set 0))

;;;; ___________________________________________________________________________

(provide 'nomis-window-backgrounds)
