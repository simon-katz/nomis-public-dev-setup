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
            repeat with _group1 in groups of _w
                tell me to logInfo("1 " & description of _group1)
                repeat with _group2 ¬
                       in groups of UI element 1 of scroll area 1 of _group1
                    tell me to logInfo("  2 " & description of _group2)
                    local _descs
                    set _descs to ""
                    set _actions to actions of _group2
                    repeat with _action in _actions
                        set _desc to description of _action
                        tell me to logInfo("    3 " & _desc)
                        set _descs to _descs & " / " & _desc
                    end repeat
                    tell me to logInfo("  2 _descs = `" & _descs & "`")
                end repeat
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
