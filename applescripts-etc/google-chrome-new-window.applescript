-- Get desktop size
tell application "Finder"
	set b to bounds of window of desktop
end tell

tell application "Google Chrome"
	make new window
        -- The following does bad things when you have multiple displays,
        -- so don't do it. SK 2019-10-14
        --
	-- set bounds of window 1 to {0, 0, 980, 2000}
	-- activate
	-- tell front window
	-- 	set bounds to b
	-- end tell
end tell
