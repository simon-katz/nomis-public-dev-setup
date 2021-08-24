-- See https://github.com/jiho/tile-windows for similar stuff.

-- Get desktop size
tell application "Finder"
    set b to bounds of window of desktop
end tell

set dd to (item 3 of b)
set d to dd/16

set curApp to (path to frontmost application as Unicode text)

tell application curApp
    tell front window
        set {x1, y1, x2, y2} to (get bounds)
        if (x1-d >= 0) then
           set bounds to {x1-d, y1, x2-d, y2}
        else
           set bounds to {0, y1, x2-x1, y2}
        end if
    end tell
end tell
