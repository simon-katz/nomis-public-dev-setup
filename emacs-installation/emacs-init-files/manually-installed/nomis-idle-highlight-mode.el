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
;;   Use `nomis/toggle-idle-highlight-colon-at-start-matters` (bound to
;;   H-q H-h H-;).
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
;;   `nomis/idle-highlight-stuff` (bound to H-q H-h H-h).

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

(defconst nomis/highlight-debug? nil)

(defun nomis/report-char-at-point (&optional msg)
  (when nomis/highlight-debug?
    (message "looking at `%s` (point = %s) [%s]"
             (let ((c (char-after)))
               (cond ((eql c ?\n)
                      "newline")
                     ((eql c nil)
                      "eof")
                     (t
                      (format "%c" c))))
             (point)
             (or msg "dunno"))))

;;;; ___________________________________________________________________________

(require 'nomis-rx)

(require 'nomis-sexp-utils)

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
  (interactive "p")
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

(defun nomis/toggle-idle-highlight-colon-at-start-matters ()
  (interactive)
  (message
   "nomis-idle-highlight-colon-at-start-matters-p = %s"
   (setq nomis-idle-highlight-colon-at-start-matters-p
         (not nomis-idle-highlight-colon-at-start-matters-p)))
  ;; When invoked with M-x, there is a delay before things change.
  ;; Something to do with waiting for idle time, I think.
  ;; (But I didn't notice this until late in th dev cycle, so maybe I changed
  ;; something.)
  ;; Anyway, force an immediate update.
  (nomis-idle-highlight-word-at-point))

;;;; ___________________________________________________________________________
;;;; Regular expressions for symbols

(defun nomis/make-char-match-regexp (str)
  (concat "["
          str
          "]"))

(defconst nomis/symbol-prefix-chars/default
  "'`#,")

(defconst nomis/symbol-prefix-chars/clojure-mode
  (concat nomis/symbol-prefix-chars/default "@^~"))


(defconst nomis/symbol-prefix-char-regexp/default
  (nomis/make-char-match-regexp nomis/symbol-prefix-chars/default))

(defconst nomis/symbol-prefix-char-regexp/incl-colon/default
  (nomis/make-char-match-regexp (concat nomis/symbol-prefix-chars/default
                                        ":")))

(defconst nomis/symbol-prefix-char-regexp/clojure-mode
  (nomis/make-char-match-regexp nomis/symbol-prefix-chars/clojure-mode))

(defconst nomis/symbol-prefix-char-regexp/incl-colon/clojure-mode
  (nomis/make-char-match-regexp (concat nomis/symbol-prefix-chars/clojure-mode
                                        ":")))

(defun nomis/symbol-prefix-char-regexp ()
  (case major-mode
    (clojure-mode
     (if nomis-idle-highlight-colon-at-start-matters-p
         nomis/symbol-prefix-char-regexp/clojure-mode
       nomis/symbol-prefix-char-regexp/incl-colon/clojure-mode))
    (t
     (if nomis-idle-highlight-colon-at-start-matters-p
         nomis/symbol-prefix-char-regexp/default
       nomis/symbol-prefix-char-regexp/incl-colon/default))))

(defconst nomis/symbol-body-chars/default
  ;; Note the position of the "-" at the beginning. So when augmenting this,
  ;; you must add at the end (otherwise you will introduce a range).
  ;; Horrible.
  "-[:alnum:]$&*+_<>/'")

(defconst nomis/symbol-body-chars/clojure-mode
  (concat nomis/symbol-body-chars/default ""))


(defconst nomis/symbol-body-char-regexp/default
  (nomis/make-char-match-regexp nomis/symbol-body-chars/default))

(defconst nomis/symbol-body-char-regexp/clojure-mode
  (nomis/make-char-match-regexp nomis/symbol-body-chars/clojure-mode))

(defun nomis/symbol-body-char-regexp ()
  (case major-mode
    (clojure-mode
     nomis/symbol-body-char-regexp/clojure-mode)
    (t
     nomis/symbol-body-char-regexp/default)))

;;;; ___________________________________________________________________________

(defun nomis-start-of-symbol-regex ()
  (apply 'concat
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
               (if (equal major-mode 'clojure-mode)
                   ;; Allow ^ before a Clojure symbol.
                   "\\^?"
                 "")
               (if nomis-idle-highlight-colon-at-start-matters-p
                   ;; If there are leading colons, our captured target
                   ;; will have it.
                   ""
                 ;; If there are leading colons, our captured target
                 ;; won't have it. But we want to allow them.
                 ":*"))))

(defconst end-of-symbol-re "\\_>")
(defconst eob-or-not-symbol-constituent-re
  (nomis/rx/or "\\'"
               (case 2
                 (1 "[^']")
                 (2
                  ;; [:word:] seems to include symbol
                  ;; chars such as $&*+-_<>.
                  "[^'[:word:]]"))))

(defun symbol-name->end-of-symbol-regex (symbol-name)
  (case 2
    (1
     end-of-symbol-re)
    (2
     (cond ;; ((not (equal major-mode 'clojure-mode))
           ;;  end-of-symbol-re)
           ;; :trailing-single-quotes
           ;; Emacs doesn't have lookahead regexes, so unfortunately we are going
           ;; to highlight the char after the symbol.
           ((s-ends-with? "'" symbol-name)
            eob-or-not-symbol-constituent-re)
           (t
            (concat end-of-symbol-re
                    eob-or-not-symbol-constituent-re))))))


(cl-defun nomis/skip-chars-forward (&rest regexps)
  (while (-any? #'looking-at-p regexps)
    (forward-char)))

(cl-defun nomis/skip-chars-backward (&rest regexps)
  (while (-any? #'looking-at-p regexps)
    (backward-char)))

(defun nomis-idle-highlight-thing ()
  (nomis/report-char-at-point "before")
  (if (nomis-looking-at-boring-place-p)
      (progn
        (nomis/report-char-at-point "boring char -- not highlighting")
        nil)
    (let* ((prefix-regexp (nomis/symbol-prefix-char-regexp))
           (body-regexp   (nomis/symbol-body-char-regexp)))
      (cl-flet ((skip-forward-prefix () (nomis/skip-chars-forward prefix-regexp))
                (skip-forward-body () (nomis/skip-chars-forward body-regexp))
                (skip-backward-all () (nomis/skip-chars-backward prefix-regexp
                                                                 body-regexp)))
        (let* ((bounds (ignore-errors
                         (save-excursion
                           ;; Move forward then back to get to start.
                           ;; This may skip over an initial colon.
                           (nomis/report-char-at-point "before")
                           
                           (unless (or (nomis-looking-at-whitespace)
                                       (nomis-looking-at-bracketed-sexp-end))
                             (skip-forward-prefix)
                             (nomis/report-char-at-point "after skip prefix")
                             (skip-forward-body)
                             (nomis/report-char-at-point "after skip body"))
                           
                           (backward-char) ; FIXME fix beginning of buffer
                           (skip-backward-all)
                           (forward-char)
                           (nomis/report-char-at-point "after going back")
                           
                           (skip-forward-prefix)
                           (nomis/report-char-at-point "after skipping prefix")
                           
                           (let* ((beg (point))
                                  (end (progn
                                         (skip-forward-prefix)
                                         (skip-forward-body)
                                         (point))))
                             (when (< beg end)
                               (cons beg end))))))
               (text
                (when bounds
                  (buffer-substring (car bounds) (cdr bounds)))))
          (when text
            (set-text-properties 0 (length text) nil text))
          text)))))



;; FIXME Don't look for strings.
;; FIXME Bug for L1 L2 L1'L2 (which was there before the new changes).


(defun nomis-idle-highlight-regexp-quote (string)
  ;; Maybe this could be simplified by using `case-fold-search` to control
  ;; the search, but I couldn't make it work.
  ;; Perhaps a bug -- see https://lists.gnu.org/archive/html/bug-gnu-emacs/2016-02/msg02002.html
  ;; JSK 2017-09-13
  (if (eq major-mode 'emacs-lisp-mode)
      (nomis/rx/or
       ;; This is only approximately correct. It doesn't work for mixed-case
       ;; things. Never mind.
       (regexp-quote (upcase string))
       (regexp-quote (downcase string)))
    (regexp-quote string)))

(defun nomis-idle-highlight-word-at-point* ()
  "Highlight the word under the point."
  (nomis/report-char-at-point "In `nomis-idle-highlight-word-at-point*`")
  (if nomis-idle-highlight-mode
      (let* ((captured-target (nomis-idle-highlight-thing)))
        (nomis-idle-highlight-unhighlight)
        (when nomis/highlight-debug?
          (message "captured-target = %s" captured-target))
        (if (or (not captured-target)
                (member captured-target
                        nomis-idle-highlight-exceptions)
                (and (eq major-mode 'org-mode)
                     (string-match-p "^\\*+$" captured-target)))
            (progn
              (when nomis/highlight-debug?
                (message "Not highlighting")))
          (progn
            (setq nomis-idle-highlight-regexp
                  (cond ((eq (string-to-char captured-target)
                             ?\")
                         (when nomis/highlight-debug?
                           (message "nomis-idle-highlight-word-at-point*: Pretty sure we can't get here."))
                         (beep)
                         (regexp-quote captured-target))
                        (t
                         (when nomis/highlight-debug?
                           (message "Looking for captured-target %s" captured-target))
                         (let* ((prefix (concat (nomis-start-of-symbol-regex)
                                                (nomis-idle-highlight-regexp-quote
                                                 captured-target)))
                                (regex-for-symbol
                                 (concat prefix
                                         (-> captured-target
                                             symbol-name->end-of-symbol-regex))))
                           (if (not (eq major-mode 'clojure-mode))
                               regex-for-symbol
                             (let* ((regex-for-use-of-ns-or-ns-alias
                                     (concat prefix "/")))
                               (nomis/rx/or regex-for-symbol
                                            regex-for-use-of-ns-or-ns-alias)))))))
            ;; (message "colon-matters-p = %s & captured-target = %s and nomis-idle-highlight-regexp = %s"
            ;;          nomis-idle-highlight-colon-at-start-matters-p
            ;;          captured-target
            ;;          nomis-idle-highlight-regexp)
            (when nomis-idle-highlight-regexp
              (when nomis/highlight-debug?
                (message "Looking for regexp %s" nomis-idle-highlight-regexp))
              (highlight-regexp nomis-idle-highlight-regexp
                                nomis-idle-highlight-face)))))))

(defun nomis-idle-highlight-word-at-point ()
  (when nomis/highlight-debug?
    (message "_____"))
  (condition-case e
      (nomis-idle-highlight-word-at-point*)
    (error
     (message "nomis-idle-highlight-word-at-point: %s"
              e))))

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

(define-key global-map (kbd "H-q H-h H-;")
  'nomis/toggle-idle-highlight-colon-at-start-matters)

;;;; ___________________________________________________________________________

(require 'nomis-hydra)

(defvar nomis/idle-highlight-stuff/initial-face-value)
(defvar nomis/idle-highlight-stuff/initial-toggle-colon-value)

(define-nomis-hydra nomis/idle-highlight-stuff
  :name-as-string "Idle Highlight Stuff"
  :key "H-q H-h H-h"
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
  (("t" nomis/toggle-idle-highlight-colon-at-start-matters
    "Toggle colon matters")
   ("<up>"     nomis-idle-highlight-cycle-up-highlight-face   "Cycle up")
   ("<down>"   nomis-idle-highlight-cycle-down-highlight-face "Cycle down")
   ("M-<up>"   nomis-idle-highlight-set-face-bright           "Bright")
   ("M-<down>" nomis-idle-highlight-set-face-muted            "Muted")))

(provide 'nomis-idle-highlight-mode)
;;; nomis-idle-highlight-mode.el ends here
