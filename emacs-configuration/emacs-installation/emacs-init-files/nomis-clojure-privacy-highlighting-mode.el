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
  `((((background dark)) ,(list :foreground "grey95"
                                :background "#0000FF"))
    (t ,(list :background (cl-case 3
                            (1 "#b3d7ff")
                            (2 "#c0f0ff")
                            (3 "#aaeeee"))
              :bold t)))
  "Face for public Clojure declarations."
  :group 'nomis/clojure-privacy-highlighting)

(defface nomis/cph/private-face
  `((((background dark)) ,(list :foreground "grey95"
                                :background "#000077"))
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

(defconst -nomis/cph/ns-qualifier-regexp
  (s-concat nomis/clojure-regexps/symbol-without-slash-regexp
            "/"))

(defconst -nomis/cph/up-to-and-incl-definer-symbol-regexp
  (s-concat "("
            -nomis/cph/whitespace-regexp/optional
            (nomis/rx/opt -nomis/cph/ns-qualifier-regexp)
            "def"
            (nomis/rx/opt nomis/clojure-regexps/symbol-without-slash-regexp)))

(defconst -nomis/cph/private-declaration-regexp
  (nomis/rx/or "-"
               (s-concat -nomis/cph/whitespace-regexp/mandatory
                         "\\^:private")))

(defconst -nomis/cph/public-regexp
  (s-concat -nomis/cph/up-to-and-incl-definer-symbol-regexp
            -nomis/cph/whitespace-regexp/mandatory
            nomis/clojure-regexps/symbol-without-slash-regexp))

(defconst -nomis/cph/private-regexp
  (s-concat -nomis/cph/up-to-and-incl-definer-symbol-regexp
            -nomis/cph/private-declaration-regexp
            -nomis/cph/whitespace-regexp/mandatory
            nomis/clojure-regexps/symbol-without-slash-regexp))

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

;;;; ___________________________________________________________________________

(provide 'nomis-clojure-privacy-highlighting-mode)
