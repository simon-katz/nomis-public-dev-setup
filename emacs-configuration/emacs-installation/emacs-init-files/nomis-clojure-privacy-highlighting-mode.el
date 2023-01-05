;;;; nomis-clojure-privacy-highlighting-mode --- Clojure privacy highlighting -*- lexical-binding: t -*-

(require 'nomis-rx)
(require 'nomis-clojure-regexps)

;;;; ___________________________________________________________________________

(defgroup nomis/clojure-privacy-highlighting nil
  "Highlight other occurrences of the word at point."
  :group 'faces)

;;;; ___________________________________________________________________________
;;;; Faces

(defface nomis/cph/public-face
  `((((background dark)) ,(list :box (list :line-width -1
                                           :color "springgreen3"
                                           :style nil)
                                ;; :foreground "grey95"
                                ;; :background "Green4"
                                ))
    (t ,(list :background (case 3
                            (1 "#b3d7ff")
                            (2 "#c0f0ff")
                            (3 "#aaeeee"))
              :bold t)))
  "Face for public Clojure declarations."
  :group 'nomis/clojure-privacy-highlighting)

(defface nomis/cph/private-face
  `((((background dark)) ,(list :box (list :line-width -1
                                           :color "salmon"
                                           :style nil)
                                ;; :foreground "grey95"
                                ;; :background "Red4"
                                ))
    (t ,(list :background "grey90")))
  "Face for private Clojure declarations."
  :group 'nomis/clojure-privacy-highlighting)

(defconst nomis/cph/public-face  'nomis/cph/public-face)
(defconst nomis/cph/private-face 'nomis/cph/private-face)

;;;; ___________________________________________________________________________
;;;; Regexps

(defconst -nomis/cph/whitespace-chars/regexp ; I can't get [[:space]] to work.
  "[ \t\r\n\v\f]")

(defconst -nomis/cph/whitespace-regexp/optional
  (s-concat -nomis/cph/whitespace-chars/regexp
            "*"))

(defconst -nomis/cph/whitespace-regexp/mandatory
  (s-concat -nomis/cph/whitespace-chars/regexp
            "+"))

(defconst -nomis/cph/begin-def-regexp
  (s-concat "\("
            -nomis/cph/whitespace-regexp/optional))

(defconst -nomis/cph/definer-symbol-regexp
  (s-concat "def"
            nomis/clojure-regexps/symbol-body-char-regexp
            "*"))

(defconst -nomis/cph/definer-symbol-not-ending-in-dash-regexp
  (nomis/rx/or
   "def"
   (s-concat -nomis/cph/definer-symbol-regexp
             nomis/clojure-regexps/symbol-body-char-except-dash-regexp)))

(defconst -nomis/cph/definer-symbol-ending-in-dash-regexp
  (s-concat -nomis/cph/definer-symbol-regexp
            "-"))

(defconst -nomis/cph/whitespace-then-symbol-regexp
  (s-concat -nomis/cph/whitespace-regexp/mandatory
            nomis/clojure-regexps/symbol-body-char-regexp
            "+"))

(defconst -nomis/cph/public-regexp
  (s-concat -nomis/cph/begin-def-regexp
            -nomis/cph/definer-symbol-not-ending-in-dash-regexp
            -nomis/cph/whitespace-then-symbol-regexp))

(defconst -nomis/cph/private-regexp
  (nomis/rx/or
   (s-concat -nomis/cph/begin-def-regexp
             -nomis/cph/definer-symbol-ending-in-dash-regexp
             -nomis/cph/whitespace-then-symbol-regexp)
   (s-concat -nomis/cph/begin-def-regexp
             -nomis/cph/definer-symbol-regexp
             -nomis/cph/whitespace-regexp/mandatory
             "\\^:private"
             -nomis/cph/whitespace-then-symbol-regexp)))

;;;; ___________________________________________________________________________
;;;; Highlighting and unhighlighting

(defun nomis/cph/unhighlight ()
  (hlt-unhighlight-region (point-min)
                          (point-max)
                          nomis/cph/public-face)
  (hlt-unhighlight-region (point-min)
                          (point-max)
                          nomis/cph/private-face))

(defun nomis/cph/highlight ()
  (nomis/cph/unhighlight)
  (when nomis/clojure-privacy-highlighting-mode
    (hlt-highlight-regexp-region (point-min)
                                 (point-max)
                                 -nomis/cph/public-regexp
                                 nomis/cph/public-face)
    (hlt-highlight-regexp-region (point-min)
                                 (point-max)
                                 -nomis/cph/private-regexp
                                 nomis/cph/private-face)))

;;;; ___________________________________________________________________________
;;;; The mode

(defcustom nomis/cph/idle-time 0.5
  "Time after which to highlight the word at point."
  :group 'nomis/clojure-privacy-highlighting
  :type 'float)

(defvar nomis/cph/global-timer nil
  "Timer to trigger privacy highlighting.")

(define-minor-mode nomis/clojure-privacy-highlighting-mode
  "Nomis-Idle-Highlight Minor Mode"
  :group 'nomis/clojure-privacy-highlighting
  (if nomis/clojure-privacy-highlighting-mode
      (progn
        (nomis/cph/highlight) ; for immediate feedback
        (unless nomis/cph/global-timer
          (setq nomis/cph/global-timer
                (run-with-idle-timer nomis/cph/idle-time
                                     :repeat 'nomis/cph/highlight))))
    (nomis/cph/unhighlight)))

(defun nomis/clojure-privacy-highlighting-mode/turn-on ()
  (nomis/clojure-privacy-highlighting-mode 1))

;;;; ___________________________________________________________________________

(provide 'nomis-clojure-privacy-highlighting-mode)
