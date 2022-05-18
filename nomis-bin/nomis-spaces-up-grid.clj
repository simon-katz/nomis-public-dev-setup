#!/usr/bin/env bb

(def ^:private n-columns 4)

(defn ^:private osa [cmd]
  (shell/sh "osascript" "-e" cmd))

(defn ^:private get-desktop-picture-filename []
  (osa "tell application \"Finder\" to get (desktop picture) as string"))

(let [current-space (->> (get-desktop-picture-filename)
                         :out
                         (re-find #"macos-desktop-backgrounds:([0-9]*)")
                         second
                         parse-long)
      new-space (max 1 (- current-space n-columns))]
  new-space
  ;; TODO: Flash screen if `(= current-space new-space)`
  ;; TODO: Move to the new space.
  )
