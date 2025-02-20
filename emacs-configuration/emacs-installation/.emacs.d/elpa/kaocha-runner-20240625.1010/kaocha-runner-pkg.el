;; -*- no-byte-compile: t; lexical-binding: nil -*-
(define-package "kaocha-runner" "20240625.1010"
  "A package for running Kaocha tests via CIDER."
  '((emacs    "26")
    (s        "1.4.0")
    (cider    "0.21.0")
    (parseedn "0.1.0"))
  :url "https://github.com/magnars/kaocha-runner.el"
  :commit "98f45ee396802c2225595c9151d4a941f9dcaa9d"
  :revdesc "98f45ee39680"
  :authors '(("Magnar Sveen" . "magnars@gmail.com"))
  :maintainers '(("Magnar Sveen" . "magnars@gmail.com")))
