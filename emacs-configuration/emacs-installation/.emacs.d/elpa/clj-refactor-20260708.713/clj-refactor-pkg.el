;; -*- no-byte-compile: t; lexical-binding: nil -*-
(define-package "clj-refactor" "20260708.713"
  "A collection of commands for refactoring Clojure code."
  '((emacs        "28.1")
    (yasnippet    "0.6.1")
    (paredit      "24")
    (clojure-mode "5.18.0")
    (cider        "1.11.1")
    (parseedn     "1.2.0")
    (transient    "0.4.1"))
  :url "https://github.com/clojure-emacs/clj-refactor.el"
  :commit "0fc45d4ca4db73d12260d1b6d80da9439abc5162"
  :revdesc "0fc45d4ca4db"
  :keywords '("convenience" "clojure" "cider")
  :authors '(("Magnar Sveen" . "magnars@gmail.com")
             ("Lars Andersen" . "expez@expez.com")
             ("Benedek Fazekas" . "benedek.fazekas@gmail.com")
             ("Bozhidar Batsov" . "bozhidar@batsov.dev"))
  :maintainers '(("Magnar Sveen" . "magnars@gmail.com")
                 ("Lars Andersen" . "expez@expez.com")
                 ("Benedek Fazekas" . "benedek.fazekas@gmail.com")
                 ("Bozhidar Batsov" . "bozhidar@batsov.dev")))
