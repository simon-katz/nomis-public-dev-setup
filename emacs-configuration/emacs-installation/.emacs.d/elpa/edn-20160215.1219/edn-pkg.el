;; -*- no-byte-compile: t; lexical-binding: nil -*-
(define-package "edn" "20160215.1219"
  "Support for reading and writing the edn data format from elisp."
  '((cl-lib "0.3")
    (emacs  "24.1")
    (peg    "0.6"))
  :url "https://www.github.com/expez/edn.el"
  :commit "be9e32d1b49e35247b263b0243df7cfdc8d413ab"
  :revdesc "be9e32d1b49e"
  :keywords '("edn" "clojure")
  :authors '(("Lars Andersen" . "expez@expez.com"))
  :maintainers '(("Lars Andersen" . "expez@expez.com")))
