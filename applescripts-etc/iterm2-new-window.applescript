-- Get desktop size
tell application "Finder"
    set {desktop_x1, desktop_y1, desktop_x2, desktop_y2} to bounds of window of desktop
end tell

--

tell application "iTerm"
    set newWindow to (create window with default profile)
    -- tell front window
    --     set {x1, y1, x2, y2} to (get bounds)
    -- end tell
    -- set bounds of window 1 to {400, 22, x2, desktop_y2}
end tell
