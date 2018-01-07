;; -*- require-final-newline: nil; -*-

:Clojure
;;;; Clojure examples for testing nomis-idle-highlight-mode.

;;; Ensure no problem at start of buffer.

;;; Move cursor through the following and check all is good.

(fred
 @fred
 :fred
 @fred
 :fred
 ^:fred foo
 #'fred
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
 fred'b
 fred'a'
 fred'a'b
 fred'&
 jim'fred
 fred'a
 fred'b
 fred'a'
 fred'a'b
 fred'&
 jim'fred

 fred/a
 fred/b
 fred/a
 fred/b)


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

fred fred' fred'' fred''' fred