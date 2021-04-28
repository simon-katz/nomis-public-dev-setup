;;; nomis/idle-highlight-mode.el --- highlight the word the point is on

;;;; ___________________________________________________________________________

;; Based on idle-highlight-mode.
;; Modifications Copyright (C) 2016 Simon Katz
;; Original licence terms apply. See below.


;; The main differences between this and the original are:
;;
;; - It uses overlays rather than text properties for the highlighting, by
;;   using the "highlight" library (`hlt-xxxx` things).
;;
;; - You can toggle whether colons at the start of a symbol are ignored. This is
;;   useful in Clojure, where sometimes a keyword and a non-keyword refer to the
;;   same thing.
;;   Use `nomis/toggle-idle-highlight-colon-at-start-matters` (bound to
;;   H-q H-h H-;).
;;
;; - You can easily switch the highlight face using:
;;   - `nomis/idle-highlight-set-face-muted`
;;   - `nomis/idle-highlight-set-face-bright`
;;   - `nomis/idle-highlight-cycle-highlight-face`
;;   - `nomis/idle-highlight-cycle-up-highlight-face`
;;   - `nomis/idle-highlight-cycle-down-highlight-face`
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

;; M-x nomis/idle-highlight-mode sets an idle timer that highlights all
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
;;   (nomis/idle-highlight-mode t))
;;
;; (add-hook 'emacs-lisp-mode-hook 'my-coding-hook)
;; (add-hook 'ruby-mode-hook 'my-coding-hook)
;; (add-hook 'js2-mode-hook 'my-coding-hook)

;;; Code:

;;;; ___________________________________________________________________________

(progn
  ;; Replace the built-in `highlight` with the manually-installed one.
  (require 'highlight))

;;;; ___________________________________________________________________________

;;;; Demo of highlighting with the libraries used by `:old` and `:new` values for
;;;; `nomis/ih/approach` behave differently.

;;;; Run the code in the comment and observe the effects on the lines marked
;;;; with (w), (x) etc.

(defmacro nomis/ih/comment (&body body)
  ;; Maybe put this somewhere general as `nomis/comment`.
  "Comment out one or more s-expressions."
  nil)

(nomis/ih/comment

 (cl-flet ((highlight (regexp face)
                      (highlight-regexp regexp face))
           (hlt-highlight (regexp face)
                          (hlt-highlight-regexp-region (point-min)
                                                       (point-max)
                                                       regexp
                                                       face)))
   (highlight     "wwww" 'hi-pink)
   (highlight     "xxxx" 'hi-green)
   (hlt-highlight " yy " 'hi-blue)
   (hlt-highlight " zz " 'hi-yellow))

 (progn
   (unhighlight-regexp "wwww")
   (unhighlight-regexp "xxxx")
   (hlt-unhighlight-region (point-min) (point-max)))

 ;; (w)  wwwwwwwwwwwwwwwwwwwwwwwwwwww <-- As expected -- all highlighted.
 ;;
 ;; (x)  xxxxxxxxxxxxxxxxxxxxxxxxxxxx <-- As expected -- all highlighted.

 ;; (y1) yy yy yy yy yy yy            <-- As expected -- need more spaces.
 ;; (y2) yy  yy  yy  yy  yy  yy       <-- As expected -- all highlighted.
 ;; (y3) yy   yy   yy   yy   yy   yy  <-- As expected -- all highlighted.
 ;;
 ;; (y1) zz zz zz zz zz zz            <-- As expected -- need more spaces.
 ;; (y2) zz  zz  zz  zz  zz  zz       <-- As expected -- all highlighted.
 ;; (y3) zz   zz   zz   zz   zz   zz  <-- As expected -- all highlighted.
 )

;;;; ___________________________________________________________________________

(defvar nomis/ih/approach :new)
(defvar nomis/ih/use-simple-regexps-p nil) ; to help with debugging

;;;; ___________________________________________________________________________

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

(defgroup nomis/idle-highlight nil
  "Highlight other occurrences of the word at point."
  :group 'faces)

;;;; ___________________________________________________________________________
;;;; Faces

(defface idle-highlight-original
  '((t (:inherit region)))
  "Face used to highlight other occurrences of the word at point."
  :group 'nomis/idle-highlight)

(defvar nomis/ih/muted-yellow "#fefd90")

(defface nomis/idle-highlight-muted
  `((((min-colors 88) (background dark))
     (:background ,nomis/ih/muted-yellow :foreground "black"))
    (((background dark)) (:background ,nomis/ih/muted-yellow :foreground "black"))
    (((min-colors 88)) (:background ,nomis/ih/muted-yellow))
    (t (:background ,nomis/ih/muted-yellow)))
  "Default face for hi-lock mode."
  :group 'hi-lock-faces)

(defvar nomis/idle-highlight-faces
  '(nomis/idle-highlight-muted
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

(defvar nomis/idle-highlight-face
  (first nomis/idle-highlight-faces))

(defun nomis/idle-highlight-report-face ()
  (interactive)
  (message "nomis/idle-highlight-face = %s (index = %s)"
           nomis/idle-highlight-face
           (position nomis/idle-highlight-face
                     nomis/idle-highlight-faces)))

(defun nomis/idle-highlight-set-face (face)
  (setq nomis/idle-highlight-face face)
  (nomis/idle-highlight-report-face))

(defun nomis/idle-highlight-set-face-muted ()
  (interactive)
  (nomis/idle-highlight-set-face 'nomis/idle-highlight-muted))

(defun nomis/idle-highlight-set-face-bright ()
  (interactive)
  (nomis/idle-highlight-set-face 'hi-yellow))

(defun nomis/idle-highlight-cycle-highlight-face (n)
  (interactive "p")
  (let* ((current-index (position nomis/idle-highlight-face
                                  nomis/idle-highlight-faces))
         (new-index (mod (+ current-index n)
                         (length nomis/idle-highlight-faces)))
         (new-face (elt nomis/idle-highlight-faces
                        new-index)))
    (nomis/idle-highlight-set-face new-face)))

(defun nomis/idle-highlight-cycle-up-highlight-face ()
  (interactive)
  (nomis/idle-highlight-cycle-highlight-face 1))

(defun nomis/idle-highlight-cycle-down-highlight-face ()
  (interactive)
  (nomis/idle-highlight-cycle-highlight-face -1))

;;;; ___________________________________________________________________________

(defcustom nomis/idle-highlight-exceptions '()
  "List of words to be excepted from highlighting."
  :group 'nomis/idle-highlight
  :type '(repeat string))

(defcustom nomis/idle-highlight-idle-time 0.5
  "Time after which to highlight the word at point."
  :group 'nomis/idle-highlight
  :type 'float)

(defvar nomis/idle-highlight-regexp nil
  "Buffer-local regexp to be nomis/idle-highlighted.")

(defvar nomis/idle-highlight-global-timer nil
  "Timer to trigger highlighting.")

;;;; ___________________________________________________________________________

(defun nomis/clojure-like-mode? (m)
  (member m
          '(clojure-mode
            clojurescript-mode
            clojurec-mode)))

;;;; ___________________________________________________________________________
;;;; Chars for symbols
;;;; - nomis/symbol-prefix-chars
;;;; - nomis/symbol-body-chars

;; (cl-loop for m in '(fred
;;                     emacs-lisp-mode
;;                     clojure-mode
;;                     yaml-mode)
;;          collect (list m
;;                        (nomis/symbol-prefix-chars m)
;;                        (nomis/symbol-body-chars m)))

(require 'cl-generic)

;;;; TODO `nomis/symbol-prefix-chars` and `nomis/symbol-body-chars` and the
;;;;      functions that build on them are misnamed.
;;;;      - They are things that go between [ and ] in a regexp.
;;;;      SO you need to be careful when changing these.

(cl-defgeneric nomis/symbol-prefix-chars (major-mode)
  "Characters other than whitespace that can prefix a symbol/identifier."
  "")

(cl-defgeneric nomis/symbol-body-chars (major-mode)
  "Characters that can be part of a symbol/identifier."
  ;; Note the position of the "-" at the beginning. So when augmenting this,
  ;; you must add at the end (otherwise you will introduce a range when creating
  ;; regexps using `nomis/rx/make-char-match-regexp/broken`).
  ;; Horrible.
  "-[:alnum:]_♭♯")

;;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(defconst nomis/symbol-prefix-chars/emacs-lisp
  "'`#,")

(defconst nomis/symbol-body-chars/emacs-lisp
  ;; Note the position of the "-" at the beginning. So when augmenting this,
  ;; you must add at the end (otherwise you will introduce a range when creating
  ;; regexps using `nomis/rx/make-char-match-regexp/broken`).
  ;; Horrible.
  "-[:alnum:]$&*+_<>/'.=?^!@~%")

;;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(defmethod nomis/symbol-prefix-chars ((major-mode (eql emacs-lisp-mode)))
  nomis/symbol-prefix-chars/emacs-lisp)

(defmethod nomis/symbol-body-chars ((major-mode (eql emacs-lisp-mode)))
  ;; Note the position of the "-" at the beginning. So when augmenting this,
  ;; you must add at the end (otherwise you will introduce a range when creating
  ;; regexps using `nomis/rx/make-char-match-regexp/broken`).
  ;; Horrible.
  nomis/symbol-body-chars/emacs-lisp)

;;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(defmethod nomis/symbol-prefix-chars ((major-mode (eql inferior-emacs-lisp-mode)))
  nomis/symbol-prefix-chars/emacs-lisp)

(defmethod nomis/symbol-body-chars ((major-mode (eql inferior-emacs-lisp-mode)))
  ;; Note the position of the "-" at the beginning. So when augmenting this,
  ;; you must add at the end (otherwise you will introduce a range when creating
  ;; regexps using `nomis/rx/make-char-match-regexp/broken`).
  ;; Horrible.
  nomis/symbol-body-chars/emacs-lisp)

;;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(defmethod nomis/symbol-prefix-chars ((major-mode (eql clojure-mode)))
  "'`#@~^")

(defmethod nomis/symbol-body-chars ((major-mode (eql clojure-mode)))
  ;; Note the position of the "-" at the beginning. So when augmenting this,
  ;; you must add at the end (otherwise you will introduce a range when creating
  ;; regexps using `nomis/rx/make-char-match-regexp/broken`).
  ;; Horrible.
  "-[:alnum:]$&*+_<>/'.=?!•●")

;;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(defmethod nomis/symbol-prefix-chars ((major-mode (eql clojurescript-mode)))
  (nomis/symbol-prefix-chars 'clojure-mode))

(defmethod nomis/symbol-body-chars ((major-mode (eql clojurescript-mode)))
  (nomis/symbol-body-chars 'clojure-mode))

;;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(defmethod nomis/symbol-prefix-chars ((major-mode (eql clojurec-mode)))
  (nomis/symbol-prefix-chars 'clojure-mode))

(defmethod nomis/symbol-body-chars ((major-mode (eql clojurec-mode)))
  (nomis/symbol-body-chars 'clojure-mode))

;;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(defmethod nomis/symbol-prefix-chars ((major-mode (eql yaml-mode)))
  "&*")

(defmethod nomis/symbol-body-chars ((major-mode (eql yaml-mode)))
  ;; Note the position of the "-" at the beginning. So when augmenting this,
  ;; you must add at the end (otherwise you will introduce a range when creating
  ;; regexps using `nomis/rx/make-char-match-regexp/broken`).
  ;; Horrible.
  "-[:alnum:]$+_<>/'.=?^")

;;;; ___________________________________________________________________________
;;;; Hacking Chars for symbols

(defvar nomis/idle-highlight-colon-at-start-matters-p
  nil)

(defun nomis/toggle-idle-highlight-colon-at-start-matters ()
  (interactive)
  (message
   "nomis/idle-highlight-colon-at-start-matters-p = %s"
   (setq nomis/idle-highlight-colon-at-start-matters-p
         (not nomis/idle-highlight-colon-at-start-matters-p)))
  ;; When invoked with M-x, there is a delay before things change.
  ;; Something to do with waiting for idle time, I think.
  ;; (But I didn't notice this until late in th dev cycle, so maybe I changed
  ;; something.)
  ;; Anyway, force an immediate update.
  (nomis/idle-highlight-word-at-point))

;;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(defvar nomis/ih/extra-body-chars "")

(defun nomis/ih/toggle-include-in-body-extras (char)
  (assert (characterp char))
  (let* ((string (string char))
         (contains? (s-contains? string nomis/ih/extra-body-chars)))
    (setq nomis/ih/extra-body-chars
          (if contains?
              (s-replace string "" nomis/ih/extra-body-chars)
            (concat string nomis/ih/extra-body-chars)))
    (message "nomis/ih/extra-body-chars = %S" nomis/ih/extra-body-chars))
  ;; When invoked with M-x, there is a delay before things change.
  ;; Something to do with waiting for idle time, I think.
  ;; (But I didn't notice this until late in th dev cycle, so maybe I changed
  ;; something.)
  ;; Anyway, force an immediate update.
  (nomis/idle-highlight-word-at-point))

(defun nomis/ih/toggle-include-in-body-extras/slash ()
  (interactive)
  (nomis/ih/toggle-include-in-body-extras ?/))

;;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(defun nomis/hi/base-chars->prefix-chars (chars)
  (concat chars
          (unless nomis/idle-highlight-colon-at-start-matters-p
            ":")))

(defun nomis/hi/base-chars->body-chars (chars)
  (concat chars
          ;; These must go at the end, because you have regexps that start
          ;; with "-".
          (when nomis/idle-highlight-colon-at-start-matters-p
            ":")
          nomis/ih/extra-body-chars))

;;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

;; FIXME Pass the major mode into these functions and through the call chain
;;       above, and memoise the top-level functions.

(defun nomis/symbol-prefix-chars/current-mode ()
  (-> (nomis/symbol-prefix-chars major-mode)
      nomis/hi/base-chars->prefix-chars))

(defun nomis/symbol-body-chars/current-mode ()
  (-> (nomis/symbol-body-chars major-mode)
      nomis/hi/base-chars->body-chars))

;;;; ___________________________________________________________________________
;;;; Regular expressions for symbols

(defun nomis/symbol-prefix-char-regexp ()
  (-> (nomis/symbol-prefix-chars/current-mode)
      nomis/rx/make-char-match-regexp/broken))

(defun nomis/symbol-body-char-regexp ()
  (-> (nomis/symbol-body-chars/current-mode)
      nomis/rx/make-char-match-regexp/broken))

(defun nomis/not-symbol-body-char-regexp ()
  (-> (nomis/symbol-body-chars/current-mode)
      nomis/rx/make-char-mismatch-regexp/broken))

;;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(defun nomis/ih/use-hack-for-symbol-boundaries? ()
  ;; "\\_<" and "\\_>" don't work well with Lispy symbols that contain
  ;; single quotes, or with yaml-mode, so we need this.
  (case 2
    (1 (or (nomis/clojure-like-mode? major-mode)
             (member major-mode
                     '(emacs-lisp-mode
                       yaml-mode))))
    (2
     ;; This gives me what I want when I have characters like ♭ and ♯ in
     ;; text documents.
     t)))

(defun nomis/hacky-non-symbol-char-regexp ()
  (assert (nomis/ih/use-hack-for-symbol-boundaries?))
  (nomis/rx/or "^"
               "$"
               (if nomis/ih/use-simple-regexps-p
                   " "
                 (nomis/not-symbol-body-char-regexp))))

;;;; ___________________________________________________________________________
;;;; Regular expressions for searching

(defun nomis/ih/regexp-quote (string)
  ;; Maybe this could be simplified by using `case-fold-search` to control
  ;; the search, but I couldn't make it work.
  ;; Perhaps a bug -- see https://lists.gnu.org/archive/html/bug-gnu-emacs/2016-02/msg02002.html
  ;; JSK 2017-09-13
  ;;
  ;; Oh, you can do `(setq font-lock-keywords-case-fold-search t)`
  ;; - that seems to be buffer-local.
  (if (and (eq major-mode 'emacs-lisp-mode)
           (not nomis/ih/use-simple-regexps-p))
      (nomis/rx/or
       ;; This is only approximately correct. It doesn't work for mixed-case
       ;; things. Never mind.
       (regexp-quote (upcase string))
       (regexp-quote (downcase string)))
    (regexp-quote string)))

(defun symbol-name->regexp-for-highlighting (symbol-name)
  (let* ((symbol-regexp (nomis/ih/regexp-quote symbol-name)))
    (if (not (nomis/ih/use-hack-for-symbol-boundaries?))
        ;; This doesn't take `nomis/idle-highlight-colon-at-start-matters-p`
        ;; into account, but I guess that's OK because we won't get here
        ;; when colons are in the language.
        (concat "\\<" symbol-regexp "\\>") ; word (not symbol) boundaries
      (let* ((non-symbol-char-regexp
              (nomis/hacky-non-symbol-char-regexp))
             (hacked-symbol-regexp
              (concat
               "'?"
               (when (and (not nomis/idle-highlight-colon-at-start-matters-p)
                          (not (string= (substring symbol-name 0 1)
                                        ":")))
                 ":?")
               symbol-regexp
               (when (nomis/clojure-like-mode? major-mode)
                 (nomis/rx/or ""
                              "/.*?" ; for namespace names or aliases
                              ))
               ;; For full stops and Clojure `Foo.`-style instance creation:
               "\\.?")))
        ;; We make our own regexps for just-before and just-after symbols, and
        ;; so we match a character before and after each symbol. This means that
        ;; two symbols separated by a single character require special
        ;; treatment.
        (concat non-symbol-char-regexp
                (nomis/rx/one-or-more
                 (concat hacked-symbol-regexp
                         non-symbol-char-regexp)))))))

;;;; ___________________________________________________________________________

(defun nomis/ih/start-of-buffer? ()
  (= (point) (point-min)))

(defun nomis/ih/end-of-buffer? ()
  (= (point) (point-max)))

(defun nomis/ih/loop (while-pred
                      stop-pred
                      action-fun)
  (cl-loop do (if (funcall while-pred)
                  (if (funcall stop-pred)
                      (cl-return :cannot-move-further)
                    (funcall action-fun))
                (cl-return nil))))

(cl-defun nomis/skip-chars-forward (&rest regexps)
  (nomis/ih/loop (lambda () (-any? #'looking-at-p regexps))
                 #'nomis/ih/end-of-buffer?
                 #'forward-char))

(cl-defun nomis/skip-chars-backward (&rest regexps)
  (nomis/ih/loop (lambda () (-any? #'looking-at-p regexps))
                 #'nomis/ih/start-of-buffer?
                 #'backward-char))

(defun nomis/remove-trailing-dot (s)
  (replace-regexp-in-string "\\.\\'" "" s))

(defun nomis/idle-highlight-thing ()
  (let* ((prefix-regexp (nomis/symbol-prefix-char-regexp))
         (body-regexp   (nomis/symbol-body-char-regexp)))
    (cl-labels
        ((looking-at-symbol-prefix? () (looking-at-p prefix-regexp))
         (looking-at-symbol-body?   () (looking-at-p body-regexp))
         (looking-at-symbol-p-or-b? () (or (looking-at-symbol-prefix?)
                                           (looking-at-symbol-body?)))
         (looking-at-symbol-p-or-b-or-just-after?
          ()
          (or (looking-at-symbol-p-or-b?)
              (unless (nomis/ih/start-of-buffer?)
                (save-excursion
                  (backward-char)
                  (looking-at-symbol-p-or-b?)))))
         (skip-forward-prefix   () (nomis/skip-chars-forward prefix-regexp))
         (skip-forward-body     () (nomis/skip-chars-forward body-regexp))
         (skip-backward-p-and-b () (nomis/skip-chars-backward prefix-regexp
                                                              body-regexp))
         (go-to-before-symbol-or-start-of-buffer
          ()
          (unless (nomis/ih/start-of-buffer?)
            (backward-char))
          (skip-backward-p-and-b))
         (go-to-beginning-of-symbol
          ()
          (let* ((pos-info (go-to-before-symbol-or-start-of-buffer)))
            (unless (eql pos-info :cannot-move-further)
              (forward-char))))
         (grab-symbol-name
          ()
          (save-excursion
            (nomis/report-char-at-point "1 before")
            (go-to-beginning-of-symbol)
            (nomis/report-char-at-point "2 after go back")
            (skip-forward-prefix)
            (nomis/report-char-at-point "3 after skip prefix")
            (let* ((beg (point))
                   (end (progn
                          (skip-forward-body)
                          (point))))
              (when (< beg end)
                (let* ((text (buffer-substring beg end)))
                  (set-text-properties 0 (length text) nil text)
                  text))))))
      (if (looking-at-symbol-p-or-b-or-just-after?)
          (let* ((symbol-name (grab-symbol-name)))
            ;; For full stops and Clojure `Foo.`-style instance creation:
            (nomis/remove-trailing-dot symbol-name))
        (progn
          (nomis/report-char-at-point "boring char -- not highlighting")
          nil)))))

(defun nomis/idle-highlight-word-at-point** ()
  (interactive)
  (when nomis/highlight-debug?
    (message "_____"))
  (let* ((captured-target (nomis/idle-highlight-thing)))
    (nomis/idle-highlight-unhighlight)
    (when nomis/highlight-debug?
      (message "captured-target = \"%s\"" captured-target))
    (if (or (not captured-target)
            (member captured-target
                    nomis/idle-highlight-exceptions)
            (and (eq major-mode 'org-mode)
                 (string-match-p "^\\*+$" captured-target)))
        (progn
          (when nomis/highlight-debug?
            (message "Not highlighting")))
      (progn
        (setq nomis/idle-highlight-regexp
              (cond ((eq (string-to-char captured-target)
                         ?\")
                     (when nomis/highlight-debug?
                       (message "nomis/idle-highlight-word-at-point*: Pretty sure we can't get here."))
                     (beep)
                     (regexp-quote captured-target))
                    (t
                     (when nomis/highlight-debug?
                       (message "Looking for captured-target \"%s\"" captured-target))
                     (-> captured-target
                         symbol-name->regexp-for-highlighting))))
        (when nomis/idle-highlight-regexp
          (when nomis/highlight-debug?
            (message "nomis/ih/approach = %s"
                     nomis/ih/approach)
            (message "colon-matters-p = %s"
                     nomis/idle-highlight-colon-at-start-matters-p)
            (message "captured-target = %s"
                     captured-target)
            (message "Looking for regexp \"%s\""
                     nomis/idle-highlight-regexp))
          (ecase nomis/ih/approach
            (:old (highlight-regexp nomis/idle-highlight-regexp
                                    nomis/idle-highlight-face))
            (:new (hlt-highlight-regexp-region (point-min)
                                               (point-max)
                                               nomis/idle-highlight-regexp
                                               nomis/idle-highlight-face))))))))

(defun nomis/idle-highlight-word-at-point* ()
  "Highlight the word under the point."
  (when nomis/idle-highlight-mode
    (nomis/idle-highlight-word-at-point**)))

(defun nomis/idle-highlight-word-at-point ()
  (condition-case e
      (nomis/idle-highlight-word-at-point*)
    (error
     (message "nomis/idle-highlight-word-at-point: %s"
              e))))

(defsubst nomis/idle-highlight-unhighlight ()
  (when nomis/idle-highlight-regexp
    (ecase nomis/ih/approach
      (:old (unhighlight-regexp nomis/idle-highlight-regexp))
      (:new (hlt-unhighlight-region (point-min)
                                    (point-max)
                                    nomis/idle-highlight-face)))
    (setq nomis/idle-highlight-regexp nil)))

(define-minor-mode nomis/idle-highlight-mode
  "Nomis-Idle-Highlight Minor Mode"
  :group 'nomis/idle-highlight
  (if nomis/idle-highlight-mode
      (progn (unless nomis/idle-highlight-global-timer
               (setq nomis/idle-highlight-global-timer
                     (run-with-idle-timer nomis/idle-highlight-idle-time
                                          :repeat 'nomis/idle-highlight-word-at-point)))
             (set (make-local-variable 'nomis/idle-highlight-regexp) nil))
    (nomis/idle-highlight-unhighlight)))

;;;; ___________________________________________________________________________

(define-key global-map (kbd "H-q H-h H-;")
  'nomis/toggle-idle-highlight-colon-at-start-matters)

;;;; ___________________________________________________________________________

(require 'nomis-hydra)

(defvar nomis/idle-highlight-stuff/initial-face-value)
(defvar nomis/idle-highlight-stuff/initial-toggle-colon-value)

(define-nomis/hydra nomis/idle-highlight-stuff
  :name-as-string "Idle Highlight Stuff"
  :key "H-q H-h H-h"
  :init-form   (progn
                 (setq nomis/idle-highlight-stuff/initial-face-value
                       nomis/idle-highlight-face)
                 (setq nomis/idle-highlight-stuff/initial-toggle-colon-value
                       nomis/idle-highlight-colon-at-start-matters-p)
                 (nomis/idle-highlight-report-face))
  :cancel-form (progn
                 (setq nomis/idle-highlight-face
                       nomis/idle-highlight-stuff/initial-face-value)
                 (setq nomis/idle-highlight-colon-at-start-matters-p
                       nomis/idle-highlight-stuff/initial-toggle-colon-value)
                 (nomis/idle-highlight-report-face))
  :hydra-heads
  (("t" nomis/toggle-idle-highlight-colon-at-start-matters
    "Toggle colon matters")
   ("<up>"     nomis/idle-highlight-cycle-up-highlight-face   "Cycle up")
   ("<down>"   nomis/idle-highlight-cycle-down-highlight-face "Cycle down")
   ("M-<up>"   nomis/idle-highlight-set-face-bright           "Bright")
   ("M-<down>" nomis/idle-highlight-set-face-muted            "Muted")))

(provide 'nomis-idle-highlight-mode)
;;; nomis/idle-highlight-mode.el ends here
