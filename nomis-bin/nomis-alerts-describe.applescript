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

set _msg_when_no_notifications to "There are no notifications to describe"

logInfo("BEGIN _________________________________")

logInfo("In nomis-alerts-expand-or-describe.applescript")

tell application "System Events"
    set _w to null
    try
        set _w to window "Notification Center" ¬
                  of application process "NotificationCenter"
    on error errMsg number errNum
        tell me to logInfo(_msg_when_no_notifications)
    end try
    if _w is not null
        try
            repeat with _group1 in groups of _w
                tell me to logInfo("1-top-level " & description of _group1)
                repeat with _item_group ¬
                       in groups of UI element 1 of scroll area 1 of _group1
                    tell me to logInfo("  --------")
                    set [_x, _y] to position of _item_group
                    tell me to logInfo("  2-item: " ¬
                                   & "y = " & _y & " " ¬
                                   & description of _item_group ¬
                                   & " " ¬
                                   & the value of static text 1 of _item_group ¬
                                   & " " ¬
                                   & the value of static text 2 of _item_group)
                    set _item_descs_as_string to ""
                    set _item_descs to {}
                    set _actions to actions of _item_group
                    repeat with _action in _actions
                        set _item_desc to description of _action
                        tell me to logInfo("    3-action: " & _item_desc)
                        set _item_descs_as_string to _item_descs_as_string & " / " & _item_desc
                        set end of _item_descs to _item_desc
                    end repeat
                    tell me to logInfo("  2-item: _item_descs_as_string = `" & _item_descs_as_string & "`")
                    tell me to logInfo("  2-item: _item_descs =" & _item_descs as text)
                end repeat
                tell me to logInfo("  --------")
            end repeat
        on error errMsg number errNum
            display dialog errMsg buttons {"OK"}
            tell me to logInfo("ERROR: Details follow...")
            set _msg to "ERROR: " & errMsg
            tell me to logInfo(_msg)
        end try
    end if
end tell

logInfo("END _________________________________")
