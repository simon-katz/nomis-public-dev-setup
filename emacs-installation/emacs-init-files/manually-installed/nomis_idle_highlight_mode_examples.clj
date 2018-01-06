;; -*- require-final-newline: nil; -*-

:Clojure
;;;; Clojure examples for testing nomis-idle-highlight-mode.

;;; Ensure no problem at start of buffer.

;;; Move cursor through the following and check all is good.

(fred
 @fred
 :fred
 ^:fred foo
 #'fred ; FIXME Doesn't highlight `fred` when cursor is on the hash. (Does in Elisp, though.) (Note that the hash is highlighted.)
 'fred
 `fred
 `fred`
 '''```fred
 ,fred
 ,fred
 ~fred
 ~fred
 "fred"
 "fred"
 FRED
 FRED
 FrEd
 FrEd

 fred
 freddy
 alfred
 fred
 fred'
 fred''
 fred'''
 fred'
 fred''
 fred'''
 fred'a
 fred'a
 fred'b
 fred'a'
 fred'a'b
 fred'&)

fred() ; FIXME No highlighting when on open parenthesis.


;;; Ensure that cursor on `fred` doesn't highlight `fred-bloggs`
fred
fred-bloggs
fred-bloggs


;;; Asterisks

*fred*
*fred*

*fred *fred()
xxxx *fred xxxx

fred* fred*()
xxxx fred* xxxx


;;; FIXME Weird bug: Move cursor up on down on first char of lines below.
;;;                  Sometimes get highlighting of `defn` and `as->` (but
;;;                  it depends on where you came from!).
(defn ^:private THIS-WAS-nsn->pieces [nsn]
  (as-> nsn __
    (name __)
    (str/split __ #"\.")
    (map symbol __)))

fred'    fred    fred