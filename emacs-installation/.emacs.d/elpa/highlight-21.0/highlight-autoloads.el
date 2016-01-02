;;; highlight-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "highlight" "highlight.el" (22152 17911 0 0))
;;; Generated autoloads from highlight.el

(defvar hlt-max-region-no-warning 100000 "\
*Maximum size (chars) of region to highlight without confirmation.
This is used only for highlighting of a regexp, which can be slow.")

(custom-autoload 'hlt-max-region-no-warning "highlight" t)

(defvar hlt-use-overlays-flag 'only "\
*Non-nil means use overlays to highlight; nil means use text properties.
This value also affects some actions, such as unhighlighting, for text
that is highlighted.  If the value is `only' (the default value), then
those actions only affect overlay highlighting.  Otherwise, they
affect both kinds of highlighting.")

(custom-autoload 'hlt-use-overlays-flag "highlight" t)

(defvar hlt-default-copy/yank-props '(face) "\
*Properties that `hlt-copy-props' and `hlt-yank-props' use by default.
You can use a prefix argument with those commands to override the
default behavior.
Either a list of properties (symbols) or `t', meaning all properties.")

(custom-autoload 'hlt-default-copy/yank-props "highlight" t)

(autoload 'hlt-choose-default-face "highlight" "\
Choose a face for highlighting.

\(fn FACE)" t nil)

(autoload 'hlt-highlighter "highlight" "\
Highlight the text you drag the mouse over.
The face used is the last face that was used for highlighting.
You can use command `hlt-choose-default-face' to choose a different face.

\(fn START-EVENT)" t nil)

(autoload 'hlt-eraser "highlight" "\
Erase highlights that you click or drag the mouse over.
If `hlt-use-overlays-flag' is non-nil, then remove overlay
highlighting for the last face that was used for highlighting.  (You
can use command `hlt-choose-default-face' first to choose a different
face.)  If `hlt-use-overlays-flag' is not `only', then remove
text-property highlighting for *ALL* faces (not just highlighting
faces).  This means, in particular, that a value of nil erases both
overlays for the last face and text properties for all faces.

\(fn START-EVENT)" t nil)

(autoload 'hlt-highlighter-mouse "highlight" "\
Same as `hlt-highlighter', but for binding to a menu item.

\(fn)" t nil)

(autoload 'hlt-eraser-mouse "highlight" "\
Same as `hlt-eraser', but for binding to a menu item.

\(fn)" t nil)

(autoload 'hlt-highlight "highlight" "\
Highlight region, regexp (PREFIX +), or unhighlight region (PREFIX -).
PREFIX arg non-negative means `hlt-highlight-regexp-region'
PREFIX arg negative means `hlt-unhighlight-region'
PREFIX arg nil means `hlt-highlight-region'.
If the region is not active or it is empty, then use the whole buffer.
The face used is the last face that was used for highlighting.
You can use command `hlt-choose-default-face' to choose a different face.

\(fn &optional PREFIX)" t nil)

(autoload 'hlt-highlight-region "highlight" "\
Highlight either the region/buffer or new input that you type.
Use the region if active, or the buffer otherwise.

If *all* of the following are true, the apply the last-used face as a
text property to the next and subsequent chars that you type, and add
that face to a facemenu menu (`Text Properties' or one of its
submenus):
 * You call this command interactively.
 * You use no prefix arg.
 * Option `prop-use-overlays-flag' is nil
 * The last property used for highlighting was `face'.

Otherwise, the behavior respects `hlt-use-overlays-flag' and depends
on the optional arguments, as follows:

 Optional args START and END are the limits of the area to act on.
  They default to the region limits.  If the region is not active or
  it is empty, then use the whole buffer.

 Optional 3rd arg FACE is the face to use.
  Interactively, this is the last face that was used for highlighting.
  (You can use command `hlt-choose-default-face' to choose a different face.)

 Optional 4th arg MSG-P non-nil means to display a progress message.
  Interactively, MSG-P is t.

Optional 5th arg MOUSE-P non-nil means use property `mouse-face', not
 `face'.  Interactively, MOUSE-P is provided by the prefix arg.

\(fn &optional START END FACE MSG-P MOUSE-P)" t nil)

(autoload 'hlt-highlight-regexp-region "highlight" "\
Highlight regular expression REGEXP in region/buffer.
Use the region if active, or the buffer otherwise.
Optional args START and END are the limits of the area to act on.
  They default to the region limits.
Optional 4th arg FACE is the face to use.
  Interactively, this is the last face that was used for highlighting.
  (You can use command `hlt-choose-default-face' to choose a different face.)
Optional 5th arg MSG-P:
  t means to treat this as an interactive call when deciding to
    display all messages.
  non-nil & non-t means to display only error and warning messages.
Optional 6th arg MOUSE-P non-nil means to use `mouse-face' property,
  not `face'.  Interactively, this is provided by the prefix arg.
Optional 7th arg NTH determines which regexp subgroup is highlighted.
  If nil or 0, the entire regexp is highlighted.  Otherwise, the NTH
  regexp subgroup (\"\\\\(...\\\\)\" expression) is highlighted.
  (NTH is not available interactively.)

\(fn &optional START END REGEXP FACE MSG-P MOUSE-P NTH)" t nil)

(autoload 'hlt-highlight-regexp-to-end "highlight" "\
Highlight text after cursor that matches REGEXP.
The behavior respects `hlt-use-overlays-flag' and depends on the
optional arguments, as follows:

 Optional 2nd arg FACE is the face to use.
  Interactively, this is the last face that was used for highlighting.
  (You can use command `hlt-choose-default-face' to choose a different
  face.)

 Optional 3rd arg MSG-P non-nil means to display a progress message.
  Interactively, MSG-P is t.

 Optional 4th arg MOUSE-P non-nil means use property `mouse-face', not
 `face'.  Interactively, MOUSE-P is provided by the prefix arg.

 Optional 5th arg NTH determines which regexp subgroup is highlighted.
  If nil or 0, the entire regexp is highlighted.  Otherwise, the NTH
  regexp subgroup (\"\\\\(...\\\\)\" expression) is highlighted.
  (NTH is not available interactively.)

\(fn REGEXP &optional FACE MSG-P MOUSE-P NTH)" t nil)

(autoload 'hlt-unhighlight-region "highlight" "\
Remove all highlighting in region or buffer.
Use the region if active, or the buffer otherwise.
The arguments are the same as those for `hlt-highlight-region'.

If `hlt-use-overlays-flag' is non-nil, then remove overlay highlighting.
If `hlt-use-overlays-flag' is not `only', then remove text-property
highlighting.  This means, in particular, that a value of nil removes
both overlays and text properties.

\(fn &optional START END FACE MSG-P MOUSE-P)" t nil)

(autoload 'hlt-unhighlight-region-for-face "highlight" "\
Remove any highlighting in the region that uses FACE.
Same as `hlt-unhighlight-region', but removes only highlighting
that uses FACE.  Interactively, you are prompted for the face.

This works only for overlay highlighting, not text-property
highlighting.

Note: When text in the region has been highlighted using more than one
face, unhighlighting for one of those faces can mean that adjacent
highlighting outside the region appears to change.  That outside text
still has the same multiple-overlay face highlighting, but the overlay
stacking order is not the same as it was.

Optional arg FACE is the face to use.
  Interactively, this is the last face that was used for highlighting.
  (You can use command `hlt-choose-default-face' to choose a different face.)
Optional args START and END are the limits of the area to act on.
  They default to the region limits.
Optional arg MOUSE-P non-nil means use `mouse-face' property, not
  `face'.  Interactively, MOUSE-P is provided by the prefix arg.

\(fn &optional FACE START END MOUSE-P)" t nil)

(autoload 'hlt-replace-highlight-face "highlight" "\
Replace OLD-FACE by NEW-FACE in overlay highlighting in the region.
This command applies only to overlay highlighting created by library
`highlight.el'.

Update the last-used highlighting face.

With a prefix argument, replace OLD-FACE as the `mouse-face' property,
 not the `face' property.

Other arguments:
 Optional args START and END are the limits of the area to act on.
  They default to the region limits.  If the region is not active or
  it is empty, then use the whole buffer.
 Optional arg MSG-P non-nil means display a progress message.
 Optional arg MOUSE-P non-nil means use `mouse-face' property, not
  `face'.  Interactively, MOUSE-P is provided by the prefix arg.

\(fn OLD-FACE NEW-FACE &optional START END MSG-P MOUSE-P)" t nil)

(autoload 'hlt-highlight-single-quotations "highlight" "\
Highlight single-quoted text in the region.
This means, for example, commands and keys between `'s: `foobar'.
If the region is not active or it is empty, then use the whole buffer.
With a prefix argument, prompt for the highlighting face to use.
Otherwise, use the last face used for highlighting.
 You can also use command `hlt-choose-default-face' to choose a different face.

\(fn &optional FACE)" t nil)

(autoload 'hlt-mouse-face-each-line "highlight" "\
Put `mouse-face' on each line of buffer in region.
If the region is active and not empty, then limit mouse-face
highlighting to the region.  Otherwise, use the whole buffer.
With a prefix argument, prompt for the highlighting face to use.
Otherwise, use the last face used for highlighting.
 You can also use command `hlt-choose-default-face' to choose a different face.
Optional args START and END are the limits of the area to act on.
  They default to the region limits.
Optional arg MSG-P non-nil means display a progress message.

\(fn &optional START END FACE MSG-P)" t nil)

(autoload 'hlt-toggle-use-overlays-flag "highlight" "\
Toggle `hlt-use-overlays-flag'.
If the current value is non-nil, it is set to nil.
If the current value is nil, it is set to the last non-nil value.

\(fn)" t nil)

(defalias 'hlt-paste-props 'hlt-yank-props)

(autoload 'hlt-yank-props "highlight" "\
Yank (paste) copied text properties over the active region.
Interactively, do nothing if there is no nonempty active region.
By default, yank only the copied properties defined by
 `hlt-default-copy/yank-props'.
With a plain or non-negative prefix arg, yank all copied properties.
With a negative prefix arg, you are prompted for the copied properties
 to yank.  To finish entering properties, hit `RET RET' (i.e., twice).

NOTE: If the list of copied text properties is empty, then yanking
      REMOVES ALL PROPERTIES from the text in the region.  This
      provides an easy way to UNpropertize text.

\(fn START END &optional ARG MSGP)" t nil)

(autoload 'hlt-mouse-copy-props "highlight" "\
Same as `hlt-copy-props', but copy at mouse pointer, not at point.

\(fn &optional EVENT ARG MSGP)" t nil)

(autoload 'hlt-copy-props "highlight" "\
Copy text properties at point for use by `hlt-yank-props'.
Properties are copied to `hlt-copied-props'.
By default, copy the properties defined by
 `hlt-default-copy/yank-props'.
With a plain or non-negative prefix arg, copy all properties.
With a negative prefix arg, you are prompted for the properties to
 copy.  To finish entering properties, hit `RET RET' (i.e., twice).

\(fn &optional POSITION ARG MSGP)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; highlight-autoloads.el ends here
