;;; nomis-idle-highlight-mode.el --- highlight the word the point is on

;;;; ___________________________________________________________________________

;; Based on idle-highlight-mode.
;; Modifications Copyright (C) 2016 Simon Katz
;; Original licence terms apply. See below.


;; The main differences between this and the original are:
;; 
;; - You can toggle whether colons at the start of a symbol are ignored. This is
;;   useful in Clojure, where sometimes a keyword and a non-keyword refer to the
;;   same thing.
;;   Use `nomis-idle-highlight-toggle-colon-at-start-matters`.
;; 
;; - You can easily switch the highlight face using:
;;   - `nomis-idle-highlight-set-face-muted`
;;   - `nomis-idle-highlight-set-face-bright`
;;   - `nomis-idle-highlight-cycle-highlight-face`
;;   - `nomis-idle-highlight-cycle-up-highlight-face`
;;   - `nomis-idle-highlight-cycle-down-highlight-face`
;; 
;; - The default highlight face is nicer (IMO).
;; 
;; - All the functionality is available from a single Hydra command,
;;   `nomis/idle-highlight-stuff` (bound to H-q H-h).

;;;; ___________________________________________________________________________

;; Copyright (C) 2008-2011 Phil Hagelberg, Cornelius Mika


;; Hack the following in case it might be used by something:
;; A_uthor: Phil Hagelberg, Cornelius Mika
;; U_RL: http://www.emacswiki.org/cgi-bin/wiki/IdleHighlight
;; P_ackage-Version: 1.1.3
;; V_ersion: 1.1.3
;; C_reated: 2008-05-13
;; K_eywords: convenience
;; E_macsWiki: IdleHighlight

;; This file is NOT part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Based on some snippets by fledermaus from the #emacs channel.

;; M-x nomis-idle-highlight-mode sets an idle timer that highlights all
;; occurences in the buffer of the word under the point.

;; Enabling it in a hook is recommended. But you don't want it enabled
;; for all buffers, just programming ones.
;;
;; Example:
;;
;; (defun my-coding-hook ()
;;   (make-local-variable 'column-number-mode)
;;   (column-number-mode t)
;;   (if window-system (hl-line-mode t))
;;   (nomis-idle-highlight-mode t))
;;
;; (add-hook 'emacs-lisp-mode-hook 'my-coding-hook)
;; (add-hook 'ruby-mode-hook 'my-coding-hook)
;; (add-hook 'js2-mode-hook 'my-coding-hook)

;;; Code:

;;;; ___________________________________________________________________________

(defgroup nomis-idle-highlight nil
  "Highlight other occurrences of the word at point."
  :group 'faces)

;;;; ___________________________________________________________________________
;;;; Faces

(defface idle-highlight-original
  '((t (:inherit region)))
  "Face used to highlight other occurrences of the word at point."
  :group 'nomis-idle-highlight)

(defvar muted-yellow "#fefd90")

(defface nomis-idle-highlight-muted
  `((((min-colors 88) (background dark))
     (:background ,muted-yellow :foreground "black"))
    (((background dark)) (:background ,muted-yellow :foreground "black"))
    (((min-colors 88)) (:background ,muted-yellow))
    (t (:background ,muted-yellow)))
  "Default face for hi-lock mode."
  :group 'hi-lock-faces)

(defvar nomis-idle-highlight-faces
  '(nomis-idle-highlight-muted
    hi-yellow
    idle-highlight-original ; clashes with region marking
    hi-pink
    hi-green
    hi-blue
    hi-black-b
    hi-blue-b
    hi-red-b
    hi-green-b
    hi-black-hb))

(defvar nomis-idle-highlight-face
  (first nomis-idle-highlight-faces))

(defun nomis-idle-highlight-report-face ()
  (interactive)
  (message "nomis-idle-highlight-face = %s (index = %s)"
           nomis-idle-highlight-face
           (position nomis-idle-highlight-face
                     nomis-idle-highlight-faces)))

(defun nomis-idle-highlight-set-face (face)
  (setq nomis-idle-highlight-face face)
  (nomis-idle-highlight-report-face))

(defun nomis-idle-highlight-set-face-muted ()
  (interactive)
  (nomis-idle-highlight-set-face 'nomis-idle-highlight-muted))

(defun nomis-idle-highlight-set-face-bright ()
  (interactive)
  (nomis-idle-highlight-set-face 'hi-yellow))

(defun nomis-idle-highlight-cycle-highlight-face (n)
  (interactive)
  (let* ((current-index (position nomis-idle-highlight-face
                                  nomis-idle-highlight-faces))
         (new-index (mod (+ current-index n)
                         (length nomis-idle-highlight-faces)))
         (new-face (elt nomis-idle-highlight-faces
                        new-index)))
    (nomis-idle-highlight-set-face new-face)))

(defun nomis-idle-highlight-cycle-up-highlight-face ()
  (interactive)
  (nomis-idle-highlight-cycle-highlight-face 1))

(defun nomis-idle-highlight-cycle-down-highlight-face ()
  (interactive)
  (nomis-idle-highlight-cycle-highlight-face -1))

;;;; ___________________________________________________________________________

(defcustom nomis-idle-highlight-exceptions '()
  "List of words to be excepted from highlighting."
  :group 'nomis-idle-highlight
  :type '(repeat string))

(defcustom nomis-idle-highlight-idle-time 0.5
  "Time after which to highlight the word at point."
  :group 'nomis-idle-highlight
  :type 'float)

(defvar nomis-idle-highlight-regexp nil
  "Buffer-local regexp to be nomis-idle-highlighted.")

(defvar nomis-idle-highlight-global-timer nil
  "Timer to trigger highlighting.")

(defvar nomis-idle-highlight-colon-at-start-matters-p
  nil)

(defun nomis-idle-highlight-toggle-colon-at-start-matters ()
  (interactive)
  (message
   "nomis-idle-highlight-colon-at-start-matters-p = %s"
   (setq nomis-idle-highlight-colon-at-start-matters-p
         (not nomis-idle-highlight-colon-at-start-matters-p))))

(defun nomis-start-of-symbol-regex ()
  (apply 'concatenate
         'string
         (list "\\_<"
               (if (equal major-mode 'clojure-mode)
                   ;; There seems to be a bug in `highlight-regexp`.
                   ;; In Clojure Mode, a regexp search for `\<_` finds
                   ;; the foo in @foo, but `highlight-regexp` does not
                   ;; find it.
                   ;; Ah! And also `highlight-symbol-at-point` doesn't
                   ;; find it.
                   ;; So:
                   "@?"
                 "")
               (if nomis-idle-highlight-colon-at-start-matters-p
                   ;; If there are leading colons, our captured target
                   ;; will have it.
                   ""
                 ;; If there are leading colons, our captured target
                 ;; won't have it. But we want to allow them.
                 ":*"))))

(defun forward-nomis-idle-highlight-thing (arg)
  "Like `forward-symbol`, but, if we land on a colon and
   `nomis-idle-highlight-colon-at-start-matters-p` is nil,
   move forward a character."
  (interactive "^p")
  (forward-symbol arg)
  (when (not nomis-idle-highlight-colon-at-start-matters-p)
    (while (looking-at-p ":")
      (forward-char))))

(require 'nomis-sexp-utils)

(defun nomis-idle-highlight-thing ()
  (when (nomis-looking-at-interesting-place)
    (let* ((bounds (ignore-errors
                     (save-excursion
                       ;; Move forward then back to get to start.
                       ;; This may skip over an initial colon.
                       (unless (or (nomis-looking-at-whitespace)
                                   (nomis-looking-at-sexp-end))
                         (forward-nomis-idle-highlight-thing 1))
                       (forward-nomis-idle-highlight-thing -1)
                       (let* ((beg (point))
                              (end (progn
                                     (forward-nomis-idle-highlight-thing 1)
                                     (point))))
                         (when (< beg end)
                           (cons beg end))))))
           (text
            (when bounds
              (buffer-substring (car bounds) (cdr bounds)))))
      (when text
        (set-text-properties 0 (length text) nil text))
      text)))

(defun nomis-idle-highlight-word-at-point ()
  "Highlight the word under the point."
  (if nomis-idle-highlight-mode
      (let* ((captured-target (nomis-idle-highlight-thing)))
        (nomis-idle-highlight-unhighlight)
        ;; (message "captured-target = %s" captured-target)
        (when (and captured-target
                   (not (member captured-target nomis-idle-highlight-exceptions)))
          (setq nomis-idle-highlight-regexp (concat (nomis-start-of-symbol-regex)
                                                    (regexp-quote captured-target)
                                                    "\\>"))
          ;; (message "colon-matters-p = %s & captured-target = %s and nomis-idle-highlight-regexp = %s"
          ;;          nomis-idle-highlight-colon-at-start-matters-p
          ;;          captured-target
          ;;          nomis-idle-highlight-regexp)
          (highlight-regexp nomis-idle-highlight-regexp
                            nomis-idle-highlight-face)))))

(defsubst nomis-idle-highlight-unhighlight ()
  (when nomis-idle-highlight-regexp
    (unhighlight-regexp nomis-idle-highlight-regexp)
    (setq nomis-idle-highlight-regexp nil)))

(define-minor-mode nomis-idle-highlight-mode
  "Nomis-Idle-Highlight Minor Mode"
  :group 'nomis-idle-highlight
  (if nomis-idle-highlight-mode
      (progn (unless nomis-idle-highlight-global-timer
               (setq nomis-idle-highlight-global-timer
                     (run-with-idle-timer nomis-idle-highlight-idle-time
                                          :repeat 'nomis-idle-highlight-word-at-point)))
             (set (make-local-variable 'nomis-idle-highlight-regexp) nil))
    (nomis-idle-highlight-unhighlight)))

;;;; ___________________________________________________________________________

(require 'nomis-hydra)

(defvar nomis/idle-highlight-stuff/initial-face-value)
(defvar nomis/idle-highlight-stuff/initial-toggle-colon-value)

(define-nomis-hydra nomis/idle-highlight-stuff
  :name-as-string "Idle Highlight Stuff"
  :key "H-q H-h"
  :init-form   (progn
                 (setq nomis/idle-highlight-stuff/initial-face-value
                       nomis-idle-highlight-face)
                 (setq nomis/idle-highlight-stuff/initial-toggle-colon-value
                       nomis-idle-highlight-colon-at-start-matters-p)
                 (nomis-idle-highlight-report-face))
  :cancel-form (progn
                 (setq nomis-idle-highlight-face
                       nomis/idle-highlight-stuff/initial-face-value)
                 (setq nomis-idle-highlight-colon-at-start-matters-p
                       nomis/idle-highlight-stuff/initial-toggle-colon-value)
                 (nomis-idle-highlight-report-face))
  :hydra-heads
  (("t" nomis-idle-highlight-toggle-colon-at-start-matters
    "Toggle colon matters")
   ("<up>"     nomis-idle-highlight-cycle-up-highlight-face   "Cycle up")
   ("<down>"   nomis-idle-highlight-cycle-down-highlight-face "Cycle down")
   ("M-<up>"   nomis-idle-highlight-set-face-bright           "Bright")
   ("M-<down>" nomis-idle-highlight-set-face-muted            "Muted")))

(provide 'nomis-idle-highlight-mode)
;;; nomis-idle-highlight-mode.el ends here
