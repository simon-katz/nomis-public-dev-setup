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
set _expand_msg                to "Expanded notifications"
set _dismiss_msg               to "Dismissed notification"
set _dismiss_all_for_app_msg   to "Dismissed all notifications for app"

--------------------------------------------------------------------------------
-- Main

logDebug("BEGIN _________________________________")

logDebug("In nomis-alerts-expand-or-describe.applescript")

set _press_desc to "press"
set _close_desc to "Close"

tell application "System Events"
    local _w
    set _w to null
    try
        set _w to window "Notification Center" ¬
                  of application process "NotificationCenter"
    on error errMsg number errNum
        tell me to logInfo(_msg_when_no_notifications)
    end try
    if _w is not null
        try

            -- See what we've got,
            -- saving to `_close_action_for_all`
            -- and       `_press_action_for_all`.
            local _close_action_for_all
            local _press_action_for_all
            set _close_action_for_all to null
            set _press_action_for_all to null
            repeat with _group1 in groups of _w
                tell me to logDebug("1-top-level " & description of _group1)
                repeat with _item_group ¬
                       in groups of UI element 1 of scroll area 1 of _group1

                    -- See what we've got for the app,
                    -- saving to `_close_action_for_app`
                    -- and       `_press_action_for_app`.
                    tell me to logDebug("  --------")
                    tell me to logDebug("  2-item: " ¬
                                   & description of _item_group ¬
                                   & " " ¬
                                   & the value of static text 1 of _item_group ¬
                                   & " " ¬
                                   & the value of static text 2 of _item_group)
                    local _app_item_descs_as_string
                    local _close_action_for_app
                    local _press_action_for_app
                    set _app_item_descs_as_string to ""
                    set _close_action_for_app to null
                    set _press_action_for_app to null
                    set _actions to actions of _item_group
                    repeat with _action in _actions
                        local _item_desc
                        set _item_desc to description of _action
                        tell me to logDebug("    3-action: " & _item_desc)
                        set _app_item_descs_as_string to _app_item_descs_as_string & " / " & _item_desc
                        if _item_desc = _press_desc then
                            set _press_action_for_app to _action
                        else if _item_desc = _close_desc then
                            set _close_action_for_app to _action
                        end if
                    end repeat
                    tell me to logDebug("  2-item: _app_item_descs_as_string = `" & _app_item_descs_as_string & "`")

                    -- Update `_close_action_for_all`
                    -- and    `_press_action_for_all`.
                    if _close_action_for_app is not null then
                        -- This is good when an app has multiple notifications
                        -- (it dismisses the top-most (most recent) one).
                        --
                        -- But when multiple apps have notifications, it
                        -- dismisses the bottom-most one. And I don't know how
                        -- we can distinguish between those situations.
                        -- Maybe just User Be Aware.
                        --
                        -- You can see /eg/:
                        --   BEGIN _________________________________
                        --   In nomis-alerts-expand-or-describe.applescript
                        --   1-top-level group
                        --     2-item: Title #371 #371 Lorem ipsum dolor sit amet
                        --     2-item: Notification #331 www.bennish.net
                        --     2-item: Title #376 #376 Lorem ipsum dolor sit amet
                        --     2-item: Notification #334 www.bennish.net
                        --   END _________________________________
                        --
                        -- The Notification Centre groups notifications by application,
                        -- but I don't know how to do that. The structure we are
                        -- navigating does not have the notion of applications.
                        set _close_action_for_all to _close_action_for_app
                    else if _press_action_for_app is not null then
                        if _press_action_for_all = null then
                            set _press_action_for_all to _press_action_for_app
                        end if
                    else
                        tell me to logInfo("SHOULD NOT GET HERE")
                    end if
                end repeat
                tell me to logDebug("  --------")
            end repeat

            -- Decide what to do, choosing a `Close` in preference to a `press`.
            local _action_to_perform
            if _close_action_for_all is not null then
                set _action_to_perform to _close_action_for_all
            else if _press_action_for_all is not null then
                set _action_to_perform to _press_action_for_all
            end

            -- Do what we've decided to do.
            if _action_to_perform = null then
                tell me to logInfo("COULD NOT WORK OUT WHAT TO DO")
            else
                perform _action_to_perform
                -- I tried extracting this to a function, but I couldn't
                -- manage it.
                -- BEGIN Want this as a separate `messageForAction` function
                local _desc, _msg
                set _desc to description of _action_to_perform
                if _desc = _press_desc then
                    set _msg to _expand_msg
                else if _desc = _close_desc then
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
            end if
        on error errMsg number errNum
            display dialog errMsg buttons {"OK"} -- Do this first because the following is flakey.
            tell me to logInfo("ERROR: Details follow...")
            local _msg
            set _msg to do shell script "echo " & quoted form of errMsg & " | nomis-remove-quotes-and-newlines"
            tell me to logInfo("ERROR: " & _msg)
        end try
    end if
end tell

logDebug("END _________________________________")
