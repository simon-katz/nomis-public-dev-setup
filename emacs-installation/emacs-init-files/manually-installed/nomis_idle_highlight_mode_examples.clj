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
 "fred"
 "fred")

fred() ; FIXME No highlighting when on open parenthesis.


;;; Bug exploration:

;;; (1)

fred-bloggs
fred
fred-bloggs

;;; (2)

*fred
*fred

*fred*
*fred*


