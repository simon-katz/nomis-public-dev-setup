#!/usr/bin/env /usr/local/bin/bb

(def space->next-space
  {1   5
   2   6
   3   7
   4   8
   5   9
   6  10
   7  11
   8  12
   9  13
   10 14
   11 15
   12 16
   13 nil
   14 nil
   15 nil
   16 nil})

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

(let [current-space (->> (get-desktop-picture-filename)
                         :out
                         (re-find #"macos-desktop-backgrounds:([0-9]*)")
                         second
                         parse-long)
      new-space (space->next-space current-space)]
  (if new-space
    (go-to-space new-space)
    ;; TODO: Flash screen if `(= current-space new-space)`
    nil)
  #_
  (shell/sh "touch"
            (str "/Users/simonkatz/development-100/repositories/zzzz-nomis-spaces-up-grid-ran-"
                 new-space))
  new-space)
