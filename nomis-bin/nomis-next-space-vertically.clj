#!/usr/bin/env /usr/local/bin/bb

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
  (let [op       (case direction :up - :down +)
        n-spaces (* n-rows n-columns)
        to       (inc (mod (dec (op from n-columns))
                           n-spaces))
        wrapped? (case direction
                   :up   (> to from)
                   :down (< to from))]
    [to
     wrapped?]))

(defn ^:private move-space-vertically [direction]
  (touch-debug-file (case direction
                      :up   "nomis-spaces-up-grid-ran"
                      :down "nomis-spaces-down-grid-ran"))
  (let [current-space (->> (get-desktop-picture-filename)
                           :out
                           (re-find #"macos-desktop-backgrounds:([0-9]*)")
                           second
                           parse-long)
        [new-space wrapped?] (next-space-details current-space
                                                 direction)]
    (touch-debug-file (format (case direction
                                :up   "nomis-spaces-up-grid-ran-%02d"
                                :down "nomis-spaces-down-grid-ran-%02d")
                              new-space))
    (go-to-space new-space)
    (when wrapped?
      (flash-screen))
    new-space))

;;;; ___________________________________________________________________________
;;;; Do stuff

(touch-debug-file (first *command-line-args*))

(move-space-vertically (case (first *command-line-args*)
                         "up"   :up
                         "down" :down))
