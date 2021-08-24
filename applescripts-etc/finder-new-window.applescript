tell application "Finder"
	set new_window to make new Finder window
	activate
	-- set the target of this_window to "/Users/simonkatz"
	set the target of the front Finder window to home
end tell