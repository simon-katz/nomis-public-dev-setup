;; -*- no-byte-compile: t; lexical-binding: nil -*-
(define-package "clj-refactor" "20250514.1903"
  "A collection of commands for refactoring Clojure code."
  '((emacs            "26.1")
    (seq              "2.19")
    (yasnippet        "0.6.1")
    (paredit          "24")
    (multiple-cursors "1.2.2")
    (clojure-mode     "5.18.0")
    (cider            "1.11.1")
    (parseedn         "1.2.0")
    (inflections      "2.6")
    (hydra            "0.13.2"))
  :url "https://github.com/clojure-emacs/clj-refactor.el"
  :commit "362cb46bf808dc42d2aaf022afe93048439680c4"
  :revdesc "362cb46bf808"
  :keywords '("convenience" "clojure" "cider")
  :authors '(("Magnar Sveen" . "magnars@gmail.com")
             ("Lars Andersen" . "expez@expez.com")
             ("Benedek Fazekas" . "benedek.fazekas@gmail.com")
             ("Bozhidar Batsov" . "bozhidar@batsov.dev"))
  :maintainers '(("Magnar Sveen" . "magnars@gmail.com")
                 ("Lars Andersen" . "expez@expez.com")
                 ("Benedek Fazekas" . "benedek.fazekas@gmail.com")
                 ("Bozhidar Batsov" . "bozhidar@batsov.dev")))
