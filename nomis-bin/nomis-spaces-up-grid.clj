#!/usr/bin/env /usr/local/bin/bb

(def space->next-space-details
  {1  [13 true]
   2  [14 true]
   3  [15 true]
   4  [16 true]
   5  [ 1 false]
   6  [ 2 false]
   7  [ 3 false]
   8  [ 4 false]
   9  [ 5 false]
   10 [ 6 false]
   11 [ 7 false]
   12 [ 8 false]
   13 [ 9 false]
   14 [10 false]
   15 [11 false]
   16 [12 false]})

(def ^:private space->key-spec
  ;; See https://eastmanreference.com/complete-list-of-applescript-key-codes
  {1  [["control"] 18] ; "1"
   2  [["control"] 19] ; "2"
   3  [["control"] 20] ; "3"
   4  [["control"] 21] ; "4"
   5  [["control"] 23] ; "5"
   6  [["control"] 22] ; "6"
   7  [["control"] 26] ; "7"
   8  [["control"] 28] ; "8"
   9  [["control"] 25] ; "9"
   10 [["control"] 29] ; "0"
   11 [["control" "option"] 18] ; "1"
   12 [["control" "option"] 19] ; "2"
   13 [["control" "option"] 20] ; "3"
   14 [["control" "option"] 21] ; "4"
   15 [["control" "option"] 23] ; "5"
   16 [["control" "option"] 22] ; "6"
   })

(defn ^:private osa [cmd]
  (shell/sh "osascript" "-e" cmd))

(defn ^:private get-desktop-picture-filename []
  (osa "tell application \"Finder\" to get (desktop picture) as string"))

(def ^:private go-to-space-format-string"
tell application \"System Events\"
        tell application \"System Events\"
                key code %s using {%s}
        end tell
end tell")

(defn ^:private go-to-space [n]
  (let [[modifier-keys key-code] (space->key-spec n)
        cmd      (format go-to-space-format-string
                         key-code
                         ;; control down, option down
                         (str/join " ,"
                                   (map #(str % " down")
                                        modifier-keys)))]
    (println "#### cmd =" (pr-str cmd))
    (println cmd)
    (osa cmd)))

(do
  (shell/sh "touch"
            "/Users/simonkatz/development-100/repositories/nomis/dev-setup/nomis-public-dev-setup/nomis-bin/zzzz-nomis-spaces-up-grid-ran")
  (let [current-space (->> (get-desktop-picture-filename)
                           :out
                           (re-find #"macos-desktop-backgrounds:([0-9]*)")
                           second
                           parse-long)
        [new-space wrapped?] (space->next-space-details current-space)]
    (shell/sh "touch"
              (str "/Users/simonkatz/development-100/repositories/nomis/dev-setup/nomis-public-dev-setup/nomis-bin/zzzz-nomis-spaces-up-grid-ran-"
                   new-space))
    (go-to-space new-space)
    (when wrapped?
      (case 5
        0 (osa "tell application \"System Events\" to repeat 2 times
key code 18 using {command down, control down, option down}
delay 0
end repeat")
        1 (osa "display alert \"wrapped\" buttons {\"Wrapped\"} giving up after 1")
        2 (osa "beep")
        ;; TODO: Not working.
        3 (shell/sh "/Users/simonkatz/development-100/repositories/nomis/dev-setup/nomis-public-dev-setup/nomis-bin/bells")
        4 (osa "display alert \"wrapped\" buttons {\"•\"} giving up after 1")
        5 (osa "display dialog \"wrapped\" buttons {\"•\"} giving up after 1")))
    new-space))