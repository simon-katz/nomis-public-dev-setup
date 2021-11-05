;;; paxedit-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "paxedit" "paxedit.el" (0 0 0 0))
;;; Generated autoloads from paxedit.el

(autoload 'paxedit-whitespace-delete-left "paxedit" "\
Delete all whitespace left of the cursor, until a non-space character
is encountered." t nil)

(autoload 'paxedit-whitespace-delete-right "paxedit" "\
Delete all whitespace right of the cursor, until a non-space character
is encountered." t nil)

(autoload 'paxedit-delete-whitespace "paxedit" "\
Delete all whitespace (e.g. space, tab, newlines) to the left and
right of the cursor.

e.g.
 ;;; Collapses the whitespace and newlines on both sides of the cursor

 This is  -!-    too much whitespace.

 ;;; ->

 This is-!-too much whitespace.

 ;;; Newlines and whitespace

 (+ 1
    -!-2)

 ;;; ->

 (+ 1-!-2)
" t nil)

(autoload 'paxedit-untabify-buffer "paxedit" "\
Remove all tabs in the buffer." t nil)

(autoload 'paxedit-indent-buffer "paxedit" "\
Re-indent buffer." t nil)

(autoload 'paxedit-cleanup "paxedit" "\
Indent buffer as defined by mode, remove tabs, and delete trialing
whitespace." t nil)

(autoload 'paxedit-goto-start-of-symbol "paxedit" "\
Move cursor to the start of the current symbol.

e.g.
" t nil)

(autoload 'paxedit-goto-end-of-symbol "paxedit" "\
Move cursor to the end of the current symbol." t nil)

(autoload 'paxedit-symbol-copy "paxedit" "\
Copy the symbol the cursor is on or next to." t nil)

(autoload 'paxedit-symbol-kill "paxedit" "\
Kill the symbol the text cursor is next to or in and cleans up the
left-over whitespace from kill." t nil)

(autoload 'paxedit-symbol-change-case "paxedit" "\
Change the symbol to all uppercase if any of the symbol characters are
lowercase, else lowercase the whole symbol.

e.g.
 artis-!-t -> ARTIS-!-T

 CL-!-AVIER -> cl-!-avier

 Tempe-!-red -> TEMPE-!-RED" t nil)

(autoload 'paxedit-symbol-occur "paxedit" "\
Search for symbol the cursor is on or next to in the current buffer
with occur." t nil)

(autoload 'paxedit-next-symbol "paxedit" "\
Go to the next symbol regardless of what may seperate the
symbols. Will move to next symbol regardless if the next
symbol is at a different depth.

e.g.
 (print-!- (* 123 456))

-> paxedit-previous-symbol

 (print (*-!- 123 456))

\(fn &optional N)" t nil)

(autoload 'paxedit-previous-symbol "paxedit" "\
Go to the previous symbol regardless of what may seperate the
symbols. Will move to previous symbol regardless if the previous
symbol is at a different depth.

e.g.
 (print (*-!- 123 456))

-> paxedit-previous-symbol

 (print-!- (* 123 456))

\(fn &optional N)" t nil)

(autoload 'paxedit-comment-align-all "paxedit" "\
Align all the comments from the point of the cursor onwards." t nil)

(autoload 'paxedit-sexp-backward-up "paxedit" "\
Go to the start of the containing parent expression.

\(fn N)" t nil)

(autoload 'paxedit-sexp-backward-end "paxedit" "\
Go to the end of the containing parent expression.

\(fn &optional N)" t nil)

(autoload 'paxedit-quoted-open-round "paxedit" "\
Insert quoted open round." t nil)

(autoload 'paxedit-open-quoted-round "paxedit" "\
Context specific single-quoted, open round. When the cursor is located within a
symbol, the symbol is wrapped in single-quoted parentheses (see scenario 1).
If the cursor is outside of any symbol a pair of single-quoted parentheses are
inserted, and a space is inserted to seperate the newly created single-quoted
parentheses from any neighboring symbols (see scenario 2). If the cursor is
located within a string a single, single-quoted, open parenthesis will be inserted
without a matching close parenthesis (see scenario 3).

Scenario 1. Located in symbol
 (a b-!-a)

 ->

 (a '(ba-!-))

Scenario 2. Located outside symbol
 (a -!-b c d)

 ->

 (a '(-!-) b c d)

Scenario 3. Located inside quotes
 (a \"some -!-string\")

 ->

 (a \"some '(-!-string\")

Scenario 4. Region has mark set
 (a b %c d%)

 ->

 (a b '(c d)-!-)
" t nil)

(autoload 'paxedit-open-round "paxedit" "\
Context specific open round. When the cursor is located within a
symbol, the symbol is wrapped in parentheses (see scenario 1). If the cursor
is outside of any symbol a pair of parentheses are inserted, and a space
is inserted to seperate the newly created parentheses from any neighboring
symbols (see scenario 2). If the cursor is located within a string a
single, open parenthesis will be inserted without a matching close
parenthesis (see scenario 3).

Scenario 1. Located in symbol
 (a b-!-a)

 ->

 (a (ba-!-))

Scenario 2. Located outside symbol
 (a -!-b c d)

 ->

 (a (-!-) b c d)

Scenario 3. Located inside quotes
 (a \"some -!-string\")

 ->

 (a \"some (-!-string\")

Scenario 4. Region has mark set
 (a b %c d%)

 ->

 (a b (c d)-!-)
" t nil)

(autoload 'paxedit-open-bracket "paxedit" "\
Context specific open bracket. When the cursor is located within a
symbol, the symbol is wrapped in brackets (see scenario 1). If the cursor
is outside of any symbol a pair of brackets are inserted, and a space
is inserted to seperate the newly created brackets from any neighboring
symbols (see scenario 2). If the cursor is located within a string a
single, open bracket will be inserted without a matching close
bracket (see scenario 3).

Scenario 1. Located in symbol
 [a b-!-a]

 ->

 [a [ba-!-]]

Scenario 2. Located outside symbol
 [a -!-b c d]

 ->

 [a [-!-] b c d]

Scenario 3. Located inside quotes
 [a \"some -!-string\"]

 ->

 [a \"some [-!-string\"]

Scenario 4. Region has mark set
 [a b %c d%]

 ->

 [a b [c d]-!-]
" t nil)

(autoload 'paxedit-open-curly "paxedit" "\
Context specific open curly bracket. When the cursor is located within a
symbol, the symbol is wrapped in curly brackets (see scenario 1). If the cursor
is outside of any symbol a pair of curly brackets are inserted, and a space
is inserted to seperate the newly created curly brackets from any neighboring
symbols (see scenario 2). If the cursor is located within a string a
single, open curly bracket will be inserted without a matching close
curly bracket (see scenario 3).

Scenario 1. Located in symbol
 {a b-!-a}

 ->

 {a {ba-!-}}

Scenario 2. Located outside symbol
 {a -!-b c d}

 ->

 {a {-!-} b c d}

Scenario 3. Located inside quotes
 {a \"some -!-string\"}

 ->

 {a \"some {-!-string\"}

Scenario 4. Region has mark set
 {a b %c d%}

 ->

 {a b {c d}-!-}
" t nil)

(autoload 'paxedit-close-sexp-newline "paxedit" "\
Close current round and newline. Faster version of the default paredit close round and newline procedure." t nil)

(autoload 'paxedit-close-sexp-newline-round "paxedit" "\
Close the current expression, create a newline, and create a new parenthesis pair." t nil)

(autoload 'paxedit-sexp-raise "paxedit" "\
Raises the expression the cursor is in while perserving the cursor location." t nil)

(autoload 'paxedit-wrap-comment "paxedit" "\
Wrap a comment macro around the current expression. If the current
expression is already wrapped by a comment, then the wrapping comment
is removed.

Comment or uncomment the expression.
 (message -!-\"hello world\") ->  (comment  (message -!-\"hello world\"))

Executing the paxedit-wrap-comment function on a commented
expression causes the comment to be removed.
 (comment  (message -!-\"hello world\")) ->  (message -!-\"hello world\")" t nil)

(autoload 'paxedit-macro-expand-replace "paxedit" "\
Replace the current expression (if there is a macro in the functional
position) with its macro expansion." t nil)

(autoload 'paxedit-sexp-close-statement "paxedit" "\
Faster version of the default paredit close round and newline procedure." t nil)

(autoload 'paxedit-function-goto-definition "paxedit" "\
Split the current window and display the definition of the function." t nil)

(autoload 'paxedit-sexp-close-newline "paxedit" "\
Faster version of the default paredit close round and newline
procedure." t nil)

(autoload 'paxedit-backward-up "paxedit" "\
Move to the start of the explicit expression, implicit expression
or comment.

Explicit expression
 (+ 1 2 (+ 3 -!-4)) -> (+ 1 2 -!-(+ 3 4))

Implicit expression

Implicit structures, Clojure maps

 {:one 1
  :two -!-2
  :three 3}

 ->

    {:one 1
  -!-:two 2
     :three 3}

In the context of a comment, the cursor will jump to the start of the
comment

 ;;; While in some comment -!-editing

 ->

 -!-;;; While in some comment editing

\(fn &optional N)" t nil)

(autoload 'paxedit-backward-end "paxedit" "\
Move to the end of the explicit expression, implicit expression or comment.

Explicit expression
 (+ 1 2 (+ 3 -!-4)) -> (+ 1 2 (+ 3 4)-!-)

Implicit expression

Implicit structures, Clojure maps

 {:one 1
  :two -!-2
  :three 3}

->

 {:one 1
  :two 2-!-
  :three 3}

In the context of a comment, the cursor will jump to the start of the comment

;;; While in some comment -!-editing

->

;;; While in some comment editing-!-

\(fn &optional N)" t nil)

(autoload 'paxedit-backward-up-2 "paxedit" "\
Go up expressions by multiples of two and place cursor at start of context.

\(fn &optional N)" t nil)

(autoload 'paxedit-backward-end-2 "paxedit" "\
Go up expressions by multiples of two and place cursor at end of context.

\(fn &optional N)" t nil)

(autoload 'paxedit-context-new-statement "paxedit" "\
Create a new SEXP depending on the context.

\(fn &optional N)" t nil)

(autoload 'paxedit-context-goto-definition "paxedit" "\
Go to the function definition." t nil)

(autoload 'paxedit-kill "paxedit" "\
Kill current explicit expression, implicit expression, or comment. Also cleans up left-over whitespace from kill and corrects indentation.

\(fn &optional N)" t nil)

(autoload 'paxedit-copy "paxedit" "\
Copy current explicit expression, implicit expression, or comment.

\(fn &optional N)" t nil)

(autoload 'paxedit-delete "paxedit" "\
Delete current explicit expression, implicit expression, or
comment. Also cleans up the left-over whitespace from deletion and
corrects indentation.

\(fn &optional N)" t nil)

(autoload 'paxedit-transpose-forward "paxedit" "\
Swap the current explicit expression, implicit expression, symbol,
or comment forward depending on what the cursor is on and what is
available to swap with. This command is very versatile and will do
the \"right\" thing in each context.

Swapping symbols, place the cursor within the symbol and run the
shortcut for paxedit-transpose-forward to swap places with the next
symbol or expression while preserving cursor and correctly
reindenting.

 (+ tw-!-o one three) -> (+ one tw-!-o three)

 (+ 1-!-0 (+ 2 3)) -> (+ (+ 2 3) 1-!-0)

Swapping expressions, place the cursor anywhere not within a symbol
and the containing expression can be swapped with the next expression.
 (concat \"-!-world!\" \"Hello \") -> (concat \"Hello \" \"-!-world!\")

 (- (+ -!-3 4) (+ 100 200)) -> (- (+ 100 200) (+ -!-3 4))

Swapped expressions are properly indented
 (if some-condition
     (-!-message \"It's false\")
   (message \"It's true\"))

 ;;; ->

 (if some-condition
     (message \"It's true\")
   (-!-message \"It's false\"))

Swapping expressions implicit structures e.g. Clojure maps
 {:two-!- 2
  :one 1
  :three 3}

 ;;; ->

 {:one 1
  :two-!- 2
  :three 3}

Swapping comments

 ;;; should be-!- last
 ;;; should be first

 ;;; ->

 ;;; should be first
 ;;; should be-!- last

\(fn &optional N)" t nil)

(autoload 'paxedit-transpose-backward "paxedit" "\
Swaps the current explicit, implicit expression, symbol, or comment
backward depending on what the cursor is on and what is available to
swap with. Swaps in the opposite direction of
`paxedit-transpose-forward', see forward documentation for examples.

\(fn &optional N)" t nil)

(autoload 'paxedit-mode "paxedit" "\
Major mode to enable Paxedit functionality.

If called interactively, enable Paxedit mode if ARG is positive,
and disable it if ARG is zero or negative.  If called from Lisp,
also enable the mode if ARG is omitted or nil, and toggle it if
ARG is `toggle'; disable the mode otherwise.

Paxedit is an Emacs extension which eliminates the work, tedium, and
mistakes involved with manual editing and refactoring LISP
code. Paxedit allows the quick refactoring of symbols, symbolic
expressions (explicit and implicit), and comments. Normally a unique
command or set of commands would allow a user to delete, copy, or
transpose symbols, symbolic expressions, or comments. Additionally,
after executing some delete or general refactoring commands the user
must clean up any extraneous whitespace, correct indentation, and make
sure all their expressions are balanced.

Paxedit takes a departure from the above manual state of code editing
through automation. Paxedit does away with multiple different
commands. Paxedit knows when it’s in a symbol or a comment. Paxedit
does the right thing in the right context. For example, Paxedit has
one delete command which can be used to delete comments and symbolic
expresions explicit and implicit. That is just one of many Paxedit’s
context aware commands. Additionally, all Paxedit commands by default
cleanup whitespace, fix indentation issues caused by refactoring, and
expressions stay balanced.

Context Navigation:

`paxedit-backward-up' - Move to the start of the explicit expression,
implicit expression or comment.

`paxedit-backward-end' - Move to the end of the explicit expression,
implicit expression or comment.

Context Refactoring:

`paxedit-transpose-forward' - Swap the current explicit expression,
implicit expression, symbol, or comment forward depending on what the
cursor is on and what is available to swap with. This command is very
versatile and will do the “right” thing in each context. See below for
the different uses.

`paxedit-transpose-backward' - Swaps the current explicit, implicit
expression, symbol, or comment backward depending on what the cursor
is on and what is available to swap with. Swaps in the opposite
direction of paxedit-transpose-forward, see forward documentation for
examples.

`paxedit-delete' - Delete current explicit expression, implicit
expression, or comment. Also cleans up the left-over whitespace from
deletion and corrects indentation.

`paxedit-kill' - Kill current explicit expression, implicit
expression, or comment. Also cleans up left-over whitespace from kill
and corrects indentation.

`paxedit-copy' - Copy current explicit expression, implicit
expression, or comment.

`paxedit-sexp-raise' - Raises the expression the cursor is in while
perserving the cursor location.

`paxedit-insert-semicolon' - Insert comment or semicolon depending on
the location (or context) of the cursor. If the cursor is in a string,
comment, or creating a character (?; in elisp or Clojure's ';') insert
semicolon else execute paredit-comment-dwim to insert comment.

`paxedit-wrap-comment' - Wrap a comment macro around the current
expression. If the current expression is already wrapped by a comment,
then the wrapping comment is removed.

Symbolic Expression Refactoring:

`paxedit-compress' - Remove all the extraneous whitespace
\(e.g. newlines, tabs, spaces) to condense expression and contained
sub-expressions onto one line.

`paxedit-dissolve' - Remove enclosing parenthesis, square brackets,
curly brackets, or string quotes. In the case of strings, the user is
prompted and asked if they would like to dissovle the enclosing quotes
since doing so could unbalance the code through introduction of rogue
parenthesis, brackets, and so on.

`paxedit-format-1' -

Symbol Refactoring:

`paxedit-symbol-change-case' - Change the symbol to all uppercase if any
of the symbol characters are lowercase, else lowercase the whole
symbol.

`paxedit-symbol-kill' - Kill the symbol the text cursor is next to or in
and cleans up the left-over whitespace from kill.

`paxedit-symbol-delete' - Delete the symbol the text cursor is next to
or in and cleans up the left-over whitespace from delete.

Debugging

`paxedit-macro-expand-replace' - Expand the current expression in its
place if it is macro.

Whitespace & Indentation:

`paxedit-cleanup' - Indent the buffer according to the rules of the
current mode.

`paxedit-delete-whitespace' - Delete all whitespace to the right and
left of the cursor.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "paxedit" '("defun-paxedit-" "paxedit-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; paxedit-autoloads.el ends here
