#!/usr/bin/env /usr/local/bin/bb

;;;; Introduction
;;;; ============

;;;; - TotalSpaces (https://totalspaces.binaryage.com/) is, or was, a great tool
;;;;   for navigating macOS Spaces (virtual desktops). It is not available for
;;;;   recent versions of macOS or recent Mac hardware.

;;;; - I hope a new version of TotalSpaces becomes available at some point.
;;;;   As of today (2022-05-21), that seems to be a possibility -- see
;;;;   https://discuss.binaryage.com/t/can-we-help-test-total-spaces-3-if-we-have-apple-silicon/8199/134

;;;; - I've hacked a replacement for parts of TotalSpaces's functionality.
;;;;   And it is very much a hack -- cobbling a bunch of things together.

;;;; - TODO: Explain the Spaces grid.

;;;; - This Script provides the ability to move up and down a grid of Spaces.
;;;;   The grid exists in your head, not in macOS, so the visual feedback is not
;;;;   ideal -- it indicates left and right movement when you've moved up
;;;;   or down.

;;;; - TODO: Explain what you've done in BetterTouchTool.
;;;;         - Not just calling this script.


;;;; Overview
;;;; ========

;;;; •••


;;;; Installation
;;;; ============

;;;; TODO: Babashka


;;;; Set Up
;;;; ======

;;;; - You need a tool to map keystrokes to actions. I use BetterTouchTool.
;;;;   Below we call this the keystroke-to-action mapper.

;;;; - Using your keystroke-to-action mapper, set up keystrokes to invoke the
;;;;   following commands:
;;;;   - <path-to-this-script> down
;;;;   - <path-to-this-script> up

;;;; - Set up keyboard shortcuts to switch between Spaces:
;;;;     System Preferences / Keyboard / Shortcuts / Mission Control
;;;;     - Switch to Desktop  1: Control-1
;;;;     - Switch to Desktop  2: Control-2
;;;;       etc
;;;;     - Switch to Desktop 10: Control-0
;;;;     - Switch to Desktop 11: Control-Option-1
;;;;     - Switch to Desktop 12: Control-Option-2
;;;;     - etc
;;;;   This script invokes those keystrokes to switch between Spaces.

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
;;;;   - TODO: Move your macos-desktop-backgrounds to this repo (this dir) so
;;;;           that other people can use them.
;;;;   - For each Space:
;;;;     - Create a desktop background picture file. The filename must include
;;;;       the Space number.
;;;;     - Set up the desktop background picture in
;;;;       System Preferences / Desktop & Screen Saver.
;;;;   - I generated the files using
;;;;     https://seotoolscentre.com/text-to-image-generator
;;;;   - I use the following filenames:
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


;;;; Notes on Use
;;;; ============

;;;; - After pressing the keys to invoke your keystroke-to-action mapper, you
;;;;   must release them so that when this script does Control-1 etc to
;;;;   do the work, no other keys are being pressed.
;;;;   TODO: Can you somehow "release" the keys in software-land?
;;;;         I think so. After all, you can press them.
;;;;         See `go-to-space-applescript-format-string`, for example.

;;;; - Don't change the order of Spaces. Because of the way we get the current
;;;;   Space number, that will break this script's idea of the grid.
;;;;   - TODO: Is there a better way to find the current Space number?
;;;;   - TODO: Maybe, if all Space movement were to come through a single
;;;;           script, you could remember the current Space. But probably that's
;;;;           a bad idea because you won't know the initial state and you
;;;;           probably won't always know when the Space changes.


;;;; Other Useful Tools
;;;; ==================

;;;; - SpaceMan (https://github.com/Jaysce/Spaceman) uses the macOS menu bar to
;;;;   show which Space is current.


;;;; Things I Would Like to Do
;;;; =========================

;;;; - TODO: Move windows between spaces.

;;;; - TODO: Do horizontal movement so that you can wrap to the same row.

;;;; - TODO: Is it possible to briefly flash an image? If so, that could be used
;;;;         to provide feedback such as flashing the current Space number or
;;;;         other graphic after moving Space.

;;;; - TODO: An overview grid. Is there somthing that does this? I don't
;;;;         think so.


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
  (str "/Users/simonkatz/development-100/repositories/nomis/dev-setup/nomis-public-dev-setup/nomis-bin/.nomis-space-debug/"))

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

(def ^:private go-to-space-applescript-format-string
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

(defn ^:private go-to-space [n]
  (let [[modifier-keys char] (space->shortcut-key-spec n)
        key-code             (char->key-code char)
        cmd                  (format go-to-space-applescript-format-string
                                     key-code
                                     ;; control down, option down
                                     (str/join " ,"
                                               (map #(str % " down")
                                                    modifier-keys)))]
    (osa cmd)))

(defn ^:private flash-screen []
  (osa flash-screen-applescript))

;;;; ___________________________________________________________________________
;;;; Our algorithm

(defn ^:private next-space-details [from direction]
  (let [n-spaces    (* n-rows n-columns)
        to          (case direction
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
        wrapped?-op (case direction
                      (:up :left)    >
                      (:down :right) <)
        wrapped?    (wrapped?-op to from)]
    [to
     wrapped?]))

(defn ^:private move-space-on-grid [direction]
  (let [filename-to-touch (str "move-space-on-grid--" (name direction))]
    (touch-debug-file filename-to-touch)
    (let [current-space (->> (get-desktop-picture-filename)
                             :out
                             (re-find #"macos-desktop-backgrounds:([0-9]*)")
                             second
                             parse-long)
          [new-space wrapped?] (next-space-details current-space
                                                   direction)]
      (touch-debug-file (str filename-to-touch "-" new-space))
      (go-to-space new-space)
      (when wrapped?
        (flash-screen))
      new-space)))

;;;; ___________________________________________________________________________
;;;; Do stuff

(move-space-on-grid (case (first *command-line-args*)
                      "up"    :up
                      "down"  :down
                      "left"  :left
                      "right" :right))

;;;; TODO: Rename file `nomis-move-space-on-grid.clj`
