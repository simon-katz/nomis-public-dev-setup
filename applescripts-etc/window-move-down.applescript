-- See https://github.com/jiho/tile-windows for similar stuff.

-- Get desktop size
tell application "Finder"
    set b to bounds of window of desktop
end tell

set dd to (item 4 of b)
set d to dd/16

set curApp to (path to frontmost application as Unicode text)

tell application curApp
    tell front window
        set {x1, y1, x2, y2} to (get bounds)
        if (y2+d <= dd) then
           set bounds to {x1, y1+d, x2, y2+d}
        else
           set bounds to {x1, dd-(y2-y1), x2, dd}
        end if
    end tell
end tell
