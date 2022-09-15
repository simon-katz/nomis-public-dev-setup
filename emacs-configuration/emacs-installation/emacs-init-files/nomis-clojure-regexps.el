;;;; nomis-clojure-regexps --- Clojure regexps -*- lexical-binding: t -*-

(require 's)
(require 'nomis-rx)

;;;; ___________________________________________________________________________
;;;; Low-level stuff

(defconst -nomis/clojure-regexps/alnum-excluding-lower-case-n
  "abcdefghijklmopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789")

(defconst -nomis/clojure-regexps/symbol-body-chars-except-dash-and-n
  (s-concat -nomis/clojure-regexps/alnum-excluding-lower-case-n
            "$&*+_<>/'.=?!•●"))

;;;; ___________________________________________________________________________
;;;; Strings of chars

(defconst nomis/clojure-regexps/symbol-body-chars-except-dash
  (s-concat "n" -nomis/clojure-regexps/symbol-body-chars-except-dash-and-n))

(defconst nomis/clojure-regexps/symbol-body-chars
  ;; Note the position of the "-" at the beginning. So when augmenting this,
  ;; you must add at the end (otherwise you will introduce a range when creating
  ;; regexps using `nomis/rx/make-char-match-regexp/broken`).
  ;; Horrible.
  (s-concat "-n" -nomis/clojure-regexps/symbol-body-chars-except-dash-and-n))

;;;; ___________________________________________________________________________
;;;; Regexps

(defconst nomis/clojure-regexps/symbol-body-char-except-dash-regexp
  (-> nomis/clojure-regexps/symbol-body-chars-except-dash
      nomis/rx/make-char-match-regexp/broken))

(defconst nomis/clojure-regexps/symbol-body-char-regexp
  (-> nomis/clojure-regexps/symbol-body-chars
      nomis/rx/make-char-match-regexp/broken))

;;;; ___________________________________________________________________________

(provide 'nomis-clojure-regexps)
