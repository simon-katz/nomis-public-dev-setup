--------------------------------------------------------------------------------
-- Logging using Hammerspoon

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

--------------------------------------------------------------------------------
-- getModifierKeys

use scripting additions
use framework "Cocoa"

on getModifierKeys() -- from https://gist.github.com/Grayson/1154126?permalink_comment_id=2345023#gistcomment-2345023
	set modifierKeysDOWN to {command_down:false, option_down:false, control_down:false, shift_down:false, fn_down:false, capslock_down:false}
	set modifierBits to current application's NSEvent's |modifierFlags|()
	set modifierBits to modifierBits * 1
	if (modifierBits > 0) then
		if (modifierBits > 8388607) then
			-- fn key is pressed, subtract it away
			set modifierBits to modifierBits - 8388608
			set fn_down of modifierKeysDOWN to true
		end if
		if (modifierBits > 1048575) then
			-- command key is pressed, subtract it away
			set modifierBits to modifierBits - 1048576
			set command_down of modifierKeysDOWN to true
		end if
		if (modifierBits > 524287) then
			-- option key is pressed, subtract it away
			set modifierBits to modifierBits - 524288
			set option_down of modifierKeysDOWN to true
		end if
		if (modifierBits > 262143) then
			-- ctrl key is pressed, subtract it away
			set modifierBits to modifierBits - 262144
			set control_down of modifierKeysDOWN to true
		end if
		if (modifierBits > 131071) then
			-- shift key is pressed, subtract it away
			set modifierBits to modifierBits - 131072
			set shift_down of modifierKeysDOWN to true
		end if
		if (modifierBits > 65535) then
			-- capslock key is pressed, subtract it away
			set modifierBits to modifierBits - 65536
			set capslock_down of modifierKeysDOWN to true
		end if
	end if
	return modifierKeysDOWN
end

--------------------------------------------------------------------------------
-- Messages

set _msg_when_no_notifications to "There are no notifications"
set _expand_msg                to "Expanding notifications"
set _dismiss_msg               to "Dismissing notification"
set _dismiss_all_for_app_msg   to "Dismissing all notifications for app"

--------------------------------------------------------------------------------
-- Main

logDebug("BEGIN _________________________________")

logDebug("In nomis-alerts-expand-or-describe.applescript")

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
            local _action_to_perform
            set _action_to_perform to null
            repeat with _group1 in groups of _w
                tell me to logDebug("1-top-level " & description of _group1)
                repeat with _group_for_app ¬
                       in groups of UI element 1 of scroll area 1 of _group1
                    tell me to logDebug("  --------")
                    tell me to logDebug("  2-app: " & description of _group_for_app)
                    local _app_item_descs
                    local _app_item_descs_as_string
                    local _close_action, _maybe_expand_action
                    set _app_item_descs to {}
                    set _app_item_descs_as_string to ""
                    set _close_action to null
                    set _maybe_expand_action to null
                    set _actions to actions of _group_for_app
                    repeat with _action in _actions
                        set _item_desc to description of _action
                        tell me to logDebug("    3-item: " & _item_desc)
                        set end of _app_item_descs to _item_desc
                        set _app_item_descs_as_string to _app_item_descs_as_string & " / " & _item_desc
                        if _item_desc = "Press" then
                            set _maybe_expand_action to _action
                        else if _item_desc = "Close" then
                            set _close_action to _action
                        end if
                    end repeat
                    tell me to logDebug("  2-app: _app_item_descs_as_string = `" & _app_item_descs_as_string & "`")
                    if "Close" is in _app_item_descs then
                        tell me to logDebug("  2-app: for closing")
                        tell me to logDebug("  2-app: " & description of _close_action)
                        set _action_to_perform to _close_action
                    else if "press" is in _app_item_descs then
                        tell me to logDebug("  2-app: for expanding")
                        tell me to logDebug("  2-app: " & description of _maybe_expand_action)
                        if _action_to_perform = null then
                            set _action_to_perform to _maybe_expand_action
                        end if
                    else
                        tell me to logInfo("SHOULD NOT GET HERE")
                    end if
                end repeat
                tell me to logDebug("  --------")
            end repeat
            if _action_to_perform = null then
                tell me to logInfo("COULD NOT WORK OUT WHAT TO DO")
            else
                -- I tried extracting this to a function, but I couldn't
                -- manage it.
                -- BEGIN Want this as a separate `messageForAction` function
                local _desc, _msg
                set _desc to description of _action_to_perform
                if _desc = "press" then
                    set _msg to _expand_msg
                else if _desc = "Close" then
                    local _modifier_keys
                    tell me to set _modifier_keys to getModifierKeys()
                    if option_down of _modifier_keys then
                        set _msg to _dismiss_all_for_app_msg
                    else
                        set _msg to _dismiss_msg
                    end if
                else
                    set _msg to "SHOULD NOT GET HERE"
                end if
                -- END Want this as a separate `messageForAction` function
                tell me to logInfo(_msg)
                perform _action_to_perform
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
