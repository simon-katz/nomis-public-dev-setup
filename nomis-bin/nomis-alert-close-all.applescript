-- Pre-Ventura:
-- Copied, with changes, from https://stackoverflow.com/questions/65143520/dismiss-macos-big-sur-notifications-with-keyboard

-- tell application "System Events"
--     tell process "NotificationCenter"
--         set theWindow to group 1 of UI element 1 of scroll area 1 of window "Notification Center"
--         # click theWindow
--         set theActions to actions of theWindow
--         repeat with theAction in theActions
--             if description of theAction is "Close" then
--                 tell theWindow
--                     perform theAction
--                 end tell
--                 exit repeat
--             end if
--         end repeat
--     end tell
-- end tell


-- Ventura:
-- Copied-and-changed from https://gist.github.com/lancethomps/a5ac103f334b171f70ce2ff983220b4f?permalink_comment_id=4534126#gistcomment-4534126

global _debugP
set _debugP to false

to displayMessage(msg)
    do shell script "~/bin-private/hs -c 'nomisMessage(\"" & msg & "\")'"
end

to logInfo(msg)
    displayMessage(msg)
end

to logDebug(msg)
    if _debugP then
        displayMessage(msg)
    end if
end

logDebug("BEGIN _________________________________")

logDebug("In nomis-alert-close-all.applescript")

logInfo("Dismissing all notifications (may not dismiss all when expanded)")

tell application "System Events"
    -- This was doing nothing except taking up several seconds.
    -- try
    --     click menu bar item 1 of menu bar 1 of application process "Control Center"
    -- end try
    try
        set _groups to groups of UI element 1 of scroll area 1 of group 1 of window "Notification Center" of application process "NotificationCenter"
        repeat with _group in _groups
            set _actions to actions of _group
            repeat with _action in _actions
                if description of _action is in {"Schlie§en", "Alle entfernen", "Close", "Clear All"} then
                    perform _action
                end if
            end repeat
        end repeat
    end try
end tell

logDebug("END _________________________________")
