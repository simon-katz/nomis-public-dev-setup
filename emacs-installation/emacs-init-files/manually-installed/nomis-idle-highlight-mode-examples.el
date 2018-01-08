;; -*- require-final-newline: nil; -*-

:Elisp
;;;; Elisp examples for testing nomis-idle-highlight-mode.

;;; Ensure no problem at start of buffer.

;;; Move cursor through the following and check all is good.

(fred
 @fred ; This is Elisp, not Clojure, so @ is not treated specially.
 :fred
 @fred
 :fred
 ^:fred foo ; This is Elisp, not Clojure, so ^ is not treated specially.
 #'fred
 'fred
 `fred
 `fred`
 '''```fred
 ,fred
 ,fred
 ~fred ; This is Elisp, not Clojure, so ~ is not treated specially.
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
 jim'fred)

fred() ; FIXME No highlighting when on open parenthesis.


;;; Run `nomis/toggle-idle-highlight-colon-at-start-matters`.
;;; Check the above again.
;;; Run `nomis/toggle-idle-highlight-colon-at-start-matters`.

;;; Put cursor at the start of the following lines. Should be no highlighting.

;(fred @fred :fred ^:fred foo)
'(fred @fred :fred ^:fred foo)
`(fred @fred :fred ^:fred foo)
#'(fred @fred :fred ^:fred foo)

;;; Highlighting happens in comments.
;;; fred @fred :fred ^:fred
;;; `fred` too.
(defvar nomis/highlight-example/dajsbjsgfhsg
  "Highlighting happens in backquotes in comments -- `fred`.")

;;; More to run through:

(defn foo-1 ()
  (let ((aaaa (goo 42 42))
        (bbbb (goo aaaa 42)))
    ;; Position cursor at start of this comment, on the first semicolon.
    ;; Ensure no highlighting.
    (goo aaaa bbbb)))

;;; Fix the things below: (there were bugs; fixed when you changed to finding
;;; symbols using your own idea of symbols prefixes and bodies)
;;;        - when on the closing parentheses
;;;        - when on some of the quotes
;;;        - when on some of the spaces

' fred

(fred  )
(fred (   ) )
(fred

 )
(fred  ')
(fred  ' )
(fred  '  )
(fred  '   )
(fred  ' )


;;; Ensure that cursor on `fred` doesn't highlight `fred-bloggs`
(fred
 fred-bloggs
 fred-bloggs)


;;; Asterisks

(*fred*
 *fred*

 *fred *fred()
 xxxx *fred xxxx

 fred* fred*()
 xxxx fred* xxxx)


;;; Other chars

(a.b
 a=b
 a?b
 a^b
 a.b
 a=b
 a?b
 a^b)

;
;;
;;;^above after the semicolons ; (and after this one too)

;; After the following symbol, check highlighting for all following points.
;; (There was a bug when at end of file, when highlighting would incorrectly
;; happen.)
final-stuff-with-blank-lines-following


