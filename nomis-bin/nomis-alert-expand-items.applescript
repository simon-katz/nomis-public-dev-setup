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

set _msg_when_no_notifications to "There are no notifications"
set _expand_msg                to "Expanding notifications"
set _msg_for_not_needed_descs  to "Notifications are already expanded (or there is a single notification)"

to actionWhenNoActionToPerform()
    logInfo("Expand: This cannot happen")
end

logDebug("BEGIN _________________________________")

logDebug("In nomis-alert-expand-items.applescript")

tell application "System Events"
    local _w
    set _w to null
    try
        -- Use window 1 rather than window "Notification Center" to avoid
        -- locale issues (e.g. "Notification Centre" in British English).
        set _w to window 1 ¬
                  of application process "NotificationCenter"
    on error errMsg number errNum
        tell me to logInfo(_msg_when_no_notifications)
    end try
    if not _w = null
        try
            local _press_action, _clear_all_action
            set _press_action     to null
            set _clear_all_action to null
            -- In Tahoe the scroll area is one level deeper than before:
            --   old: scroll area 1 of _group1
            --   new: scroll area 1 of group 1 of _group1
            repeat with _group1 in groups of _w
                repeat with _group2 ¬
                       in groups of group 1 of scroll area 1 of group 1 of _group1
                    set _actions to actions of _group2
                    repeat with _action in _actions
                        set _desc to description of _action
                        if _desc = "press" then
                            set _press_action to _action
                        else if _desc = "Clear All" then
                            set _clear_all_action to _action
                        end if
                    end repeat
                end repeat
            end repeat
            -- Only expand if notifications are stacked. A stack has "Clear All"
            -- whereas individual (already expanded) notifications have "Close".
            if _clear_all_action is not null then
                if _press_action is null then
                    tell me to actionWhenNoActionToPerform()
                else
                    tell me to logInfo(_expand_msg)
                    perform _press_action
                end if
            else
                tell me to logInfo(_msg_for_not_needed_descs)
            end if
        on error errMsg number errNum
            display dialog errMsg buttons {"OK"}
            tell me to logInfo("ERROR: Details follow...")
            set _msg to "ERROR: " & errMsg
            tell me to logInfo(_msg)
        end try
    end if
end tell

logDebug("END _________________________________")
