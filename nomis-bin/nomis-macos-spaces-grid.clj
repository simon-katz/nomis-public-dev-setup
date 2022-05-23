#!/usr/bin/env /usr/local/bin/bb

;;;; Introduction
;;;; ============

;;;; - TotalSpaces (https://totalspaces.binaryage.com/) is, or was, a great tool
;;;;   for navigating macOS Spaces (virtual desktops), but it is not available
;;;;   for recent versions of macOS or recent Mac hardware.

;;;; - A key feature of TotalSpaces is a grid of Spaces. (macOS used to have
;;;;   this functionality built in until the early 2010s, until El Capitan,
;;;;   I think, but for some reason it was removed.)

;;;; - I hope a new version of TotalSpaces becomes available at some point.
;;;;   As of today (2022-05-21), that seems to be a possibility -- see
;;;;   https://discuss.binaryage.com/t/can-we-help-test-total-spaces-3-if-we-have-apple-silicon/8199/134

;;;; - Even though macOS doesn't support a Spaces grid, we can still have a grid
;;;;   in our head. I have 16 Spaces. In macOS that's just a linear sequence of
;;;;   Spaces, but in my head it's a 4 x 4 grid. (If you don't want 4 x 4, You
;;;;   can easily change things below to have a different size and shape grid.)
;;;;
;;;;   My notional grid looks like this:
;;;;
;;;;        ----+----+----+----
;;;;       |  1 |  2 |  3 |  4 |
;;;;       |----+----+----+----|
;;;;       |  5 |  6 |  7 |  8 |
;;;;       |----+----+----+----|
;;;;       |  9 | 10 | 11 | 12 |
;;;;       |----+----+----+----|
;;;;       | 13 | 14 | 15 | 16 |
;;;;        ----+----+----+----

;;;; - This script provides grid-aware functionality for moving left, right, up
;;;;   and down through the notional Spaces grid. When moving left from the
;;;;   leftmost column or right from the rightmost column, it wraps and stays in
;;;;   the same row. When moving up from the topmost row or down from the
;;;;   bottom-most row, it wraps and stays in the same column.

;;;; - The visual feedback is not ideal because macOS doesn't understand the
;;;;   grid -- it indicates left and right movement when you've moved up
;;;;   or down.
;;;;   (Maybe I can find a way to improve that.)


;;;; See Also
;;;; ========

;;;; You might want to consider Keyboard Maestro.
;;;;
;;;; I took a look at it and some Space-switching functionlity that is build
;;;; using it, but I found that functionlity to be very slow and I gave up
;;;; on it.
;;;;
;;;; It seems that Keyboard Maestro way be the eay to go if you want more
;;;; control. I came across a couple of things (maybe related):
;;;;
;;;; https://forum.keyboardmaestro.com/t/move-frontmost-window-to-a-different-space/10512
;;;; The macro moves the mouse to the toolbar of the window, performs
;;;; a click-and-hold, and activates one of Mission Control’s Space movement
;;;; shortcuts (see below). After that, it moves the mouse back to the
;;;; previous position.
;;;;
;;;; https://forum.keyboardmaestro.com/t/macros-desktop-spaces-macros-to-improve-navigation-and-window-management-v1-1/27033
;;;; MACROS: Desktop Spaces • Macros to Improve Navigation and Window Management, v1.1
;;;;
;;;; Both look like things you could modify to use the keystrokes you want, get wrapping,
;;;; get nice animations.


;;;; Suggested Keystrokes
;;;; ====================

;;;; (You will need a keystroke-to-action mapper. In this section I talk about
;;;; keystrokes without saying how they are mapped to actions. See the Set Up
;;;; section for that.)

;;;; I suggest using keystrokes that reinforce the notional grid by using
;;;; a grid of keys on the keyboard.

;;;; I use the following as my grid:
;;;;
;;;;         7 8 9 0
;;;;         U I O P
;;;;         J K L ;
;;;;         M , . /

;;;; To switch to a Space, I use Control-Option-Command-<X>, where:
;;;; - <X> can be one of the keys in the grid above, to move directly to a Space.
;;;;   Call these the alphanumeric keystrokes.
;;;; - <X> can be a cursor key, to move up/down/left/right.
;;;;   Call these the cursor keystrokes.

;;;; To move a window to a Space, I add the Fn key to the alphanumeric
;;;; keystrokes. I don't have this functionality with the cursor keystrokes.
;;;; Maybe I can add that later.


;;;; Babashka
;;;; ========

;;;; This script is written in Babashka (https://github.com/babashka/babashka).

;;;; To install Babashka, see https://github.com/babashka/babashka#installation


;;;; Set Up
;;;; ======

;;;; - This script invokes macOS functionality to switch Spaces using the
;;;;   keystrokes that are defined in System Preferences / Keyboard / Shortcuts
;;;;   / Mission Control.

;;;;   If you want to use this script as-is, set things as follows:
;;;;
;;;;     - Switch to Desktop  1: Control-1
;;;;     - Switch to Desktop  2: Control-2
;;;;     - Switch to Desktop  3: Control-3
;;;;     - Switch to Desktop  4: Control-4
;;;;     - Switch to Desktop  5: Control-5
;;;;     - Switch to Desktop  6: Control-6
;;;;     - Switch to Desktop  7: Control-7
;;;;     - Switch to Desktop  8: Control-8
;;;;     - Switch to Desktop  9: Control-9
;;;;     - Switch to Desktop 10: Control-0
;;;;     - Switch to Desktop 11: Control-Option-1
;;;;     - Switch to Desktop 12: Control-Option-2
;;;;     - Switch to Desktop 13: Control-Option-3
;;;;     - Switch to Desktop 14: Control-Option-4
;;;;     - Switch to Desktop 15: Control-Option-5
;;;;     - Switch to Desktop 16: Control-Option-6
;;;;
;;;;   If you want different keystrokes you will need to edit this script.

;;;; - Copy this script to somewhere on your computer. If it is not executable,
;;;;   make it so (`chmod +x <filename>`).

;;;; - You need a tool to map keystrokes to actions -- a keystroke-to-action
;;;;   mapper. I use BetterTouchTool, and here I talk about how I set that up.

;;;; - For the keystrokes defined in the Suggested Keystrokes section:
;;;;   - Alphanumeric keystrokes: I use built-in BetterTouchTool functionality.
;;;;   - Cursor keystrokes: I invoke this script using
;;;;     BetterTouchTool's "Execute Terminal Command (Async, non-blocking)"
;;;;     action type to invoke the foilowing commands:
;;;;       <full-path-to-this-script> down
;;;;       <full-path-to-this-script> up
;;;;       <full-path-to-this-script> left
;;;;       <full-path-to-this-script> right

;;;; - When wrapping across the top or bottom of the Spaces grid, this script
;;;;   tries to flash the screen. If you want that to happen, turn on the
;;;;   following: System Preferences / Accessibility / Audio / Flash the screen
;;;;   when an alert sound occurs

;;;; - Current Space Number
;;;;   - This script needs a way to find the current Space number.
;;;;   - It seems that recent versions of AppleScript don't provide this.
;;;;   - Maybe there's a better way, but for now at least we set up each Space
;;;;     to have a unique background image, and we use that to identify the
;;;;     Space. /NB/ This means that if you change the order of Spaces you will
;;;;     break this script's idea of the grid.
;;;;   - We got the idea from https://stackoverflow.com/a/34691386/2148181
;;;;   - For each Space:
;;;;     - You need a desktop background picture file. The filename must include
;;;;       the Space number.
;;;;     - Set up the desktop background picture in
;;;;       System Preferences / Desktop & Screen Saver.
;;;;   - I use the files in the `macos-desktop-backgrounds` subdirectory where
;;;;     this script is. My files have the following names:
;;;;        1.png
;;;;        2.png
;;;;        3.png
;;;;        4.png
;;;;        5.png
;;;;        6.png
;;;;        7.png
;;;;        8.png
;;;;        9.png
;;;;        10.png
;;;;        11.png
;;;;        12.png
;;;;        13.png
;;;;        14.png
;;;;        15.png
;;;;        16.png
;;;;   - I generated the files using
;;;;     https://seotoolscentre.com/text-to-image-generator


;;;; Notes on Use
;;;; ============

;;;; - After pressing the keys to invoke your keystroke-to-action mapper, you
;;;;   must release them so that when this script does Control-1 etc to
;;;;   do the work, no other keys are being pressed.

;;;; - Don't change the order of Spaces. Because of the way we get the current
;;;;   Space number, that will break this script's idea of the grid.


;;;; Other Useful Tools
;;;; ==================

;;;; - WhichSpace

;;;; - SpaceMan (https://github.com/Jaysce/Spaceman) uses the macOS menu bar to
;;;;   show which Space is current.


;;;; Things I Would Like to Do
;;;; =========================

;;;; TODO: Maybe use yabai to switch Spaces.
;;;;       It has `yabai -m space --focus 1` etc.
;;;;       - See
;;;;         https://github.com/koekeishiya/yabai/wiki/Commands#space-commands
;;;;
;;;;       Oh, I just got "cannot focus space due to an error with the
;;;;       scripting-addition."
;;;;       - Fixed with:
;;;;             brew services stop yabai
;;;;             sudo yabai --load-sa
;;;;             brew services start yabai
;;;;       - TODO: Notice when the Space doesn't change (may need try...catch to
;;;;               not exit), and issue an alert saying to fix -- and maybe fix
;;;;               automatically -- notifying when finished.
;;;;
;;;;       This approach has several advantages:
;;;;       - You don't need to set up keystrokes in Mission Control.
;;;;       - You don't need to release keys to allow the Mission Control
;;;;         keystrokes to be invoked.
;;;;       - It doesn't use animations, so when you move up it doesn't show that
;;;;         you've moved left/right.

;;;; TODO: Move windows between spaces.

;;;; TODO: Is it possible to briefly flash an image? If so, that could be used
;;;;       to provide feedback such as flashing the current Space number or
;;;;       other graphic after moving Space.

;;;; TODO: Display of an overview grid. Is there something that does this?
;;;;       I don't think so.

;;;; TODO: Can you somehow "release" the keys in software-land?
;;;;       I think so. After all, you can press them.
;;;;       See `make-space-current-format-string`, for example.

;;;; TODO: Is there a better way to find the current Space number?
;;;;
;;;;       Maybe, if all Space movement were to come through a single script,
;;;;       you could remember the current Space. But probably that's a bad idea
;;;;       because you won't know the initial state and you probably won't
;;;;       always know when the Space changes.
;;;;
;;;;       Apparently the Total Maestro macros you looked at briefly use
;;;;       WhichSpace via AppleScript to find the current Space number.
;;;;       But I looked at that using ScriptEditor which claimed there was no
;;;;       AppleScript interface.


;;;; ___________________________________________________________________________
;;;; Stuff that other people might want to change.

(def ^:private n-rows    4)
(def ^:private n-columns 4)

(def ^:private space->shortcut-key-spec
  {1  [["control"] \1]
   2  [["control"] \2]
   3  [["control"] \3]
   4  [["control"] \4]
   5  [["control"] \5]
   6  [["control"] \6]
   7  [["control"] \7]
   8  [["control"] \8]
   9  [["control"] \9]
   10 [["control"] \0]
   11 [["control" "option"] \1]
   12 [["control" "option"] \2]
   13 [["control" "option"] \3]
   14 [["control" "option"] \4]
   15 [["control" "option"] \5]
   16 [["control" "option"] \6]})

;;;; ___________________________________________________________________________
;;;; Support for debugging

(def ^:private debug? false)

(def ^:private debug-dir
  (str "/Users/simonkatz/nomis-bin/.nomis-space-debug/"))

;;;; ___________________________________________________________________________
;;;; Utility functions

(defn ^:private touch-debug-file [name]
  (when debug?
    (shell/sh "mkdir"
              "-p"
              debug-dir)
    (shell/sh "touch"
              (str debug-dir name))))

(def ^:private char->key-code
  ;; See https://eastmanreference.com/complete-list-of-applescript-key-codes
  {\1 18
   \2 19
   \3 20
   \4 21
   \5 23
   \6 22
   \7 26
   \8 28
   \9 25
   \0 29})

;;;; ___________________________________________________________________________
;;;; AppleScript

(def ^:private make-space-current-format-string
  "
tell application \"System Events\"
        tell application \"System Events\"
                key code %s using {%s}
        end tell
end tell")

(def ^:private flash-screen-applescript
  "
set curVolume to get volume settings

if output muted of curVolume is false then
    set volume with output muted
end if

beep 1

if output muted of curVolume is false then
    set volume without output muted
end if
")

(defn ^:private osa [cmd]
  (shell/sh "osascript" "-e" cmd))

(defn ^:private get-desktop-picture-filename []
  (osa "tell application \"Finder\" to get (desktop picture) as string"))

(defn ^:private make-space-current [n]
  (case 2
    1 (let [[modifier-keys char] (space->shortcut-key-spec n)
            key-code             (char->key-code char)
            cmd                  (format make-space-current-format-string
                                         key-code
                                         ;; control down, option down
                                         (str/join " ,"
                                                   (map #(str % " down")
                                                        modifier-keys)))]
        (osa cmd))
    2 (shell/sh "/opt/homebrew/bin/yabai" "-m" "space" "--focus" (str n))))

(defn ^:private flash-screen []
  (osa flash-screen-applescript))

;;;; ___________________________________________________________________________
;;;; Our algorithm

(defn ^:private next-space-details [from command]
  (let [n-spaces    (* n-rows n-columns)
        to          (case command
                      :up   (inc (mod (dec (- from n-columns))
                                      n-spaces))
                      :down (inc (mod (dec (+ from n-columns))
                                      n-spaces))
                      :left (if (zero? (mod (dec from) n-columns))
                              (+ from (dec n-columns))
                              (dec from))
                      :right (if (zero? (mod from n-columns))
                               (- from (dec n-columns))
                               (inc from)))
        wrapped?-op (case command
                      (:up :left)    >
                      (:down :right) <)
        wrapped?    (wrapped?-op to from)]
    [to
     wrapped?]))

(defn ^:private nomis-macos-spaces-grid [command]
  (let [filename-to-touch (str "nomis-macos-spaces-grid--" (name command))]
    (touch-debug-file filename-to-touch)
    (let [current-space (->> (get-desktop-picture-filename)
                             :out
                             (re-find #"macos-desktop-backgrounds:([0-9]*)")
                             second
                             parse-long)
          [new-space wrapped?] (next-space-details current-space
                                                   command)]
      (touch-debug-file (str filename-to-touch "-" new-space))
      (make-space-current new-space)
      (when wrapped?
        (flash-screen))
      new-space)))

;;;; ___________________________________________________________________________
;;;; Do stuff

(nomis-macos-spaces-grid (keyword (first *command-line-args*)))
