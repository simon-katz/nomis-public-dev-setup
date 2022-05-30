#!/usr/bin/env /opt/homebrew/bin/bb

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

;;;; - We support two ways of invoking the underlying functionality for
;;;;   switching between Spaces. The default is to use yabai. The alternative is
;;;;   to make use of keystrokes that are defined in Mission Control.
;;;;   See the "Notes on `:yabai` vs `:control-1-etc`" section to help you
;;;;   decide which approach is right for you.


;;;; Notes on `:yabai` vs `:control-1-etc`
;;;; =====================================

;;;; With `:yabai`:
;;;; - You have to install and set up yabai.

;;;; With `:control-1-etc`
;;;; - You have to set up keystrokes in Mission Control.
;;;; - You get misleading visual feedback because macOS doesn't understand the
;;;;   grid -- it indicates left and right movement when you've moved up or
;;;;   down. yabai doesn't use animations.
;;;; - After pressing the keys to invoke your keystroke-to-action mapper, you
;;;;   must release them so that when this script does Control-1 etc to do the
;;;;   work, no other keys are being pressed.


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

;;;; If you are using a Mac with an Intel CPU Intel Mac, you need to change the
;;;; first line of this script to:
;;;;     #!/usr/bin/env /usr/local/bin/bb
;;;; That's because Homebrew installs executables in different locations for
;;;; Intel And Silicon Macs


;;;; General Set Up
;;;; ==============

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
;;;;   - Maybe there's a better way, but for now at least you must set up each
;;;;     Space to have a unique background image, and we use that to identify
;;;;     the Space. /NB/ This means that if you change the order of Spaces you
;;;;     will break this script's idea of the grid.
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


;;;; Additional Set Up for `approach-for-make-space-current` = `:yabai`
;;;; =================================================================

;;;; If you want to use yabai to invoke the underlying functionality for
;;;; switching between Spaces, follow the instructions in this section.

;;;; See https://github.com/koekeishiya/yabai.

;;;; The minimum yabai set up is:
;;;; - Install yabai.
;;;; - Set up yabai scripting.
;;;; - Run the yabai service.

;;;; You may want to set up yabai in the same way as me:
;;;; - I don't use its window management functionality.
;;;; - I do use its window-border-coloring functionality.
;;;; - My `yabairc` file is as follows (remove the ;;;; and whitespace at the
;;;;   start of each line):
;;;;     --------8<-------- BEGIN my `yabairc` file
;;;;     # Disable window management.
;;;;     yabai -m config layout float
;;;;
;;;;     # Enable window borders.
;;;;     yabai -m config window_border on
;;;;     yabai -m config window_border_width 6
;;;;     yabai -m config active_window_border_color 0xFF0000FF   # - was 0XFF50FA7B
;;;;     yabai -m config normal_window_border_color 0x00112233 # transparent - was 0xFFBD93F9
;;;;     --------8<-------- END my `yabairc` file


;;;; Additional Set Up for `approach-for-make-space-current` = `:control-1-etc`
;;;; ==========================================================================

;;;; If you want to use Mission Control keystrokes to invoke the underlying
;;;; functionality for switching between Spaces, follow the instructions in
;;;; this section.

;;;; Change the value of `approach-for-make-space-current` (below) to
;;;; `:control-1-etc`.

;;;; In System Preferences / Keyboard / Shortcuts / Mission Control, set things
;;;; as follows:
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
;;;; You can use different keystrokes, but then you would have to edit this
;;;; script to invoke those.


;;;; Notes on Use
;;;; ============

;;;; Don't change the order of Spaces. Because of the way we get the current
;;;; Space number, that will break this script's idea of the grid.


;;;; Other Useful Tools
;;;; ==================

;;;; - WhichSpace

;;;; - SpaceMan (https://github.com/Jaysce/Spaceman) uses the macOS menu bar to
;;;;   show which Space is current.


;;;; Things I Would Like to Do
;;;; =========================

;;;; TODO: Move windows between spaces.
;;;;       Ah! `yabai -m window --space 2`
;;;;       - And this reliably moves my Emacs windows, unlike BetterTouchTool.

;;;; TODO: Is it possible to briefly flash an image? If so, that could be used
;;;;       to provide feedback such as flashing the current Space number or
;;;;       other graphic after moving Space.

;;;; TODO: Display of an overview grid. Is there something that does this?
;;;;       I don't think so.

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

(def ^:private approach-for-make-space-current
  ;; One of `:yabai` or `:control-1-etc`.
  :yabai)

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

(defn ^:private make-space-current [n move-window?]
  (case approach-for-make-space-current
    :control-1-etc
    (let [[modifier-keys char] (space->shortcut-key-spec n)
          key-code             (char->key-code char)
          cmd                  (format make-space-current-format-string
                                       key-code
                                       ;; control down, option down
                                       (str/join " ,"
                                                 (map #(str % " down")
                                                      modifier-keys)))]
      (osa cmd))
    ;;
    :yabai
    (do
      ;; TODO: Design the command-line args properly.
      ;; TODO: Mention "/opt/homebrew" and alternatives in documentation.
      ;;       Store that in a var.
      (when move-window?
        (shell/sh "/opt/homebrew/bin/yabai" "-m" "window" "--space" (str n)))
      (shell/sh "/opt/homebrew/bin/yabai" "-m" "space" "--focus" (str n)))))

(defn ^:private flash-screen []
  (osa flash-screen-applescript))

;;;; ___________________________________________________________________________
;;;; Our algorithm

(defn ^:private next-space-details [current-space command]
  (let [n-spaces    (* n-rows n-columns)
        to          (case command
                      :up   (inc (mod (dec (- current-space n-columns))
                                      n-spaces))
                      :down (inc (mod (dec (+ current-space n-columns))
                                      n-spaces))
                      :left (if (zero? (mod (dec current-space) n-columns))
                              (+ current-space (dec n-columns))
                              (dec current-space))
                      :right (if (zero? (mod current-space n-columns))
                               (- current-space (dec n-columns))
                               (inc current-space)))
        wrapped?-op (case command
                      (:up :left)    >
                      (:down :right) <)
        wrapped?    (wrapped?-op to current-space)]
    [to
     (when wrapped? :wrapped)]))

(defn ^:private goto-space [current-space]
  (let [n (parse-long (second *command-line-args*))]
    [n
     (when (= current-space n)
       :same-space)]))

(defn ^:private space->feedback-filename [space]
  (format "/Users/simonkatz/development-100/repositories/nomis/dev-setup/nomis-public-dev-setup/nomis-bin/macos-desktop-backgrounds/feedback-%s.png"
          space))

(defn ^:private flash-one-picture [new-space]
  (shell/sh "sh"
            "-c"
            (format "bash <<EOF
                       qlmanage -p %s &
                       sleep 0.7
                       kill %%1
                     EOF"
                    (space->feedback-filename new-space))))

(defn ^:private flash-two-pictures [old-space new-space]
  (shell/sh "sh"
            "-c"
            (format "bash <<EOF
                       qlmanage -p %s &
                       sleep 0.2
                       qlmanage -p %s &
                       sleep 0.5
                       kill %%1
                       kill %%2
                     EOF"
                    (space->feedback-filename old-space)
                    (space->feedback-filename new-space))))

(defn ^:private nomis-macos-spaces-grid [command]
  (let [filename-to-touch (str "nomis-macos-spaces-grid--" (name command))
        move-window? ((set *command-line-args*) "move-window")]
    (touch-debug-file filename-to-touch)
    (let [current-space (->> (get-desktop-picture-filename)
                             :out
                             (re-find #"macos-desktop-backgrounds:([0-9]*)")
                             second
                             parse-long)
          [new-space special-info] (case command
                                     (:up :down :left :right)
                                     (next-space-details current-space command)
                                     ;;
                                     ;; TODO: We're not using this, right?
                                     :goto-space
                                     (goto-space current-space))]
      (touch-debug-file (str filename-to-touch "-" new-space))
      (if move-window?
        (flash-one-picture new-space)
        ;; With move-window, `flash-two-pictures` breaks things -- the window
        ;; often gets left behind.
        (flash-two-pictures current-space new-space))
      (make-space-current new-space move-window?)
      #_(condp = special-info
          :wrapped (flash-screen)
          :same-space (flash-screen)
          nil)
      (touch-debug-file (str filename-to-touch "-" new-space "-done"))
      new-space)))

;;;; ___________________________________________________________________________
;;;; Do stuff

(nomis-macos-spaces-grid (keyword (first *command-line-args*)))
