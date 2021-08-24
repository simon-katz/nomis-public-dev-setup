if application "ForkLift" is not running then
    activate application "ForkLift"
else
    activate application "ForkLift"

    delay 0.1 # Sometimes the keystroke goes to the old current app, so try this

    #### NOT WORKING -- the Command-n goes to the old current app too often

    tell application "System Events"
        tell process "ForkLift"
            keystroke "n" using command down
        end tell
    end tell
end if
