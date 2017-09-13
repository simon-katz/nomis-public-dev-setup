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
 FrEd)

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


