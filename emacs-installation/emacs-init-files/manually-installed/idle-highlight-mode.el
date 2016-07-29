;;; idle-highlight-mode.el --- highlight the word the point is on



;; Modifications Copyright (C) 2016 Simon Katz
;; Original licence terms apply. See below.



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

;; M-x idle-highlight-mode sets an idle timer that highlights all
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
;;   (idle-highlight-mode t))
;;
;; (add-hook 'emacs-lisp-mode-hook 'my-coding-hook)
;; (add-hook 'ruby-mode-hook 'my-coding-hook)
;; (add-hook 'js2-mode-hook 'my-coding-hook)

;;; Code:

(require 'thingatpt)


(defgroup idle-highlight nil
 "Highlight other occurrences of the word at point."
 :group 'faces)

(defface idle-highlight
 '((t (:inherit region)))
 "Face used to highlight other occurrences of the word at point."
 :group 'idle-highlight)

(defcustom idle-highlight-exceptions '()
  "List of words to be excepted from highlighting."
  :group 'idle-highlight
  :type '(repeat string))

(defcustom idle-highlight-idle-time 0.5
  "Time after which to highlight the word at point."
  :group 'idle-highlight
  :type 'float)

(defvar idle-highlight-regexp nil
 "Buffer-local regexp to be idle-highlighted.")

(defvar idle-highlight-global-timer nil
 "Timer to trigger highlighting.")

(progn
  (defvar nomis-idle-highlight-colon-at-start-matters-p
    nil)

  (defun nomis-idle-highlight-toggle-colon-at-start-matters-p ()
    (interactive)
    (message
     "New value = %s"
     (setq nomis-idle-highlight-colon-at-start-matters-p
           (not nomis-idle-highlight-colon-at-start-matters-p))))

  (defun nomis-start-of-symbol-regex ()
    (case 3
      (1 "\\<")
      (2 "\\<@?")
      (3 (apply 'concatenate
                'string
                (list "\\_<"
                      (progn
                        ;; There seems to be a bug in `highlight-regexp`.
                        ;; In Clojure Mode, a regexp search for `\<_` finds
                        ;; the foo in @foo, but `highlight-regexp` does not
                        ;; find it.
                        ;; Ah! And also `highlight-symbol-at-point` doesn't
                        ;; find it.
                        ;; So:
                        "@?")
                      (if nomis-idle-highlight-colon-at-start-matters-p
                          ;; If there is a leading colon, our captured target
                          ;; will have it.
                          ""
                        ;; If there is a leading colon, our captured target
                        ;; won't have it. But we want to allow one.
                        ":?")))))))

(defvar nomis-idle-highlight
  (case 2
    ( 1 'idle-highlight)
    ( 2 'hi-yellow)
    ( 3 'hi-pink)
    ( 4 'hi-green)
    ( 5 'hi-blue)
    ( 6 'hi-black-b)
    ( 7 'hi-blue-b)
    ( 8 'hi-red-b)
    ( 9 'hi-green-b)
    (10 'hi-black-hb)))

(defun forward-nomis-idle-highlight-thing (arg)
  "Like `forward-symbol`, but, if we land on a colon and
   `nomis-idle-highlight-colon-at-start-matters-p` is nil,
   move forward a character."
  (interactive "^p")
  (forward-symbol arg)
  (when (and (not nomis-idle-highlight-colon-at-start-matters-p)
             (looking-at-p ":"))
    (forward-char)))

(defun idle-highlight-word-at-point ()
  "Highlight the word under the point."
  (if idle-highlight-mode
      (let* ((captured-target (thing-at-point 'nomis-idle-highlight-thing t)))
        (idle-highlight-unhighlight)
        ;; (message "captured-target = %s" captured-target)
        (when (and captured-target
                   (not (member captured-target idle-highlight-exceptions)))
          (setq idle-highlight-regexp (concat (nomis-start-of-symbol-regex)
                                              (regexp-quote captured-target)
                                              "\\>"))
          ;; (message "colon-matters-p = %s & captured-target = %s and idle-highlight-regexp = %s"
          ;;          nomis-idle-highlight-colon-at-start-matters-p
          ;;          captured-target
          ;;          idle-highlight-regexp)
          (highlight-regexp idle-highlight-regexp
                            nomis-idle-highlight)))))

(defsubst idle-highlight-unhighlight ()
  (when idle-highlight-regexp
    (unhighlight-regexp idle-highlight-regexp)
    (setq idle-highlight-regexp nil)))

;;;###autoload
(define-minor-mode idle-highlight-mode
  "Idle-Highlight Minor Mode"
  :group 'idle-highlight
  (if idle-highlight-mode
      (progn (unless idle-highlight-global-timer
               (setq idle-highlight-global-timer
                     (run-with-idle-timer idle-highlight-idle-time
                                          :repeat 'idle-highlight-word-at-point)))
             (set (make-local-variable 'idle-highlight-regexp) nil))
    (idle-highlight-unhighlight)))

(provide 'idle-highlight-mode)
;;; idle-highlight-mode.el ends here
