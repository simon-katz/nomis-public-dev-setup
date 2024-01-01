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

set _msg_for_normal_action     to "Dismissing notification(s)"
set _msg_when_no_notifications to "There are no notifications to dismiss"
set _msg_for_not_needed_descs  to "Close: This cannot happen"
set _actions_of_interest       to {"Close"}
set _needed_descs to null -- anything

to actionWhenNoActionToPerform()
    do shell script "nomis-alert-expand-items"
end

logDebug("BEGIN _________________________________")

logDebug("In nomis-alerts-expand-or-dismiss.applescript")

tell application "System Events"
    local _w
    set _w to null
    try
        set _w to window "Notification Center" ¬
                  of application process "NotificationCenter"
    on error errMsg number errNum
        tell me to logInfo(_msg_when_no_notifications)
    end try
    if not _w = null
        try
            local _action_to_perform, _descs
            set _action_to_perform to null
            set _descs to ""
            repeat with _group1 in groups of _w
                repeat with _group2 ¬
                       in groups of UI element 1 of scroll area 1 of _group1
                    set _actions to actions of _group2
                    repeat with _action in _actions
                        set _desc to description of _action
                        set _descs to _descs & " / " & _desc
                        if _desc is in _actions_of_interest then
                            set _action_to_perform to _action
                        end if
                    end repeat
                end repeat
            end repeat
            if _needed_descs = null or _descs = _needed_descs then
                if _action_to_perform = null then
                    tell me to actionWhenNoActionToPerform()
                else
                    tell me to logInfo(_msg_for_normal_action)
                    perform _action_to_perform
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
