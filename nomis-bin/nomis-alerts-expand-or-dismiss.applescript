--------------------------------------------------------------------------------
-- Logging using Hammerspoon

set _debugP to false

to displayMessage(msg)
    do shell script "~/bin-private/hs -c 'nomisMessage(\"" & msg & "\")'"
end displayMessage

to logInfo(msg)
    displayMessage(msg)
end logInfo

to logDebug(msg)
    if my _debugP then
        displayMessage(msg)
    end if
end logDebug

--------------------------------------------------------------------------------
-- isIn
-- An `is in` that works for a list being in a list of lists.

to isIn(_item, _list)
    repeat with _list_item in _list
        set _equal to (_item = contents of _list_item)
        if _equal then
            return true
        end if
    end repeat
    return false
end isIn

--------------------------------------------------------------------------------
-- getModifierKeys

use scripting additions
use framework "Cocoa"

to getModifierKeys() -- from https://gist.github.com/Grayson/1154126?permalink_comment_id=2345023#gistcomment-2345023
    set modifierKeysDOWN to {command_down:false, ¬
                             option_down:false, ¬
                             control_down:false, ¬
                             shift_down:false, ¬
                             fn_down:false, ¬
                             capslock_down:false}
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
end getModifierKeys

--------------------------------------------------------------------------------
-- Messages

set _msg_when_no_notifications to "There are no notifications"
set _expand_msg                to "Expanded notifications"
set _dismiss_msg               to "Dismissed notification"
set _dismiss_all_for_app_msg   to "Dismissed all notifications for app"
set _dismiss_all_for_app_2_msg to "Dismissed all notifications for app (even though collapsed)"

--------------------------------------------------------------------------------
-- messageForAction

set _press_desc to "press"
set _close_desc to "Close"
set _clear_all_desc to "Clear All"

to messageForAction(_desc_of_action, _option_down_p)
    if _desc_of_action = my _press_desc then
        set _msg to my _expand_msg
    else if _desc_of_action = my _close_desc then
        if _option_down_p then -- Using Option on Close does a Close All
            set _msg to my _dismiss_all_for_app_msg
        else
            set _msg to my _dismiss_msg
        end if
    else if _desc_of_action = my _clear_all_desc then
        set _msg to my _dismiss_all_for_app_2_msg
    else
        set _msg to "SHOULD NOT GET HERE"
    end if
    return _msg
end messageForAction

--------------------------------------------------------------------------------
-- Action item descriptions

-- Script Editor and Safari
set _item_descs_for_collapsed_001 to ¬
{"AXScrollToVisible", "press", "Show Details", "Show", "Clear All"}

-- Chrome and Vivaldi
set _item_descs_for_collapsed_002 to ¬
{"AXScrollToVisible", "press", "Show Details", "Settings", "Clear All"}

-- Slack
set _item_descs_for_collapsed_003 to ¬
{"AXScrollToVisible", "press", "Show Details", "Close", "Reply", "Clear All"}

set _item_descs_s_for_collapsed_apps to ¬
{_item_descs_for_collapsed_001, ¬
 _item_descs_for_collapsed_002, ¬
 _item_descs_for_collapsed_003}

--------------------------------------------------------------------------------
-- Main

-- The approach:
--   - Find the top-most item (using y coordinate).
--   - If it has "press" and not "Close", we have a collection
--     of notifications. Do "press" to expand it.
--   - If it has "Close", we have a single notification. Do "Close" to close
--     it.
--   - Otherwise, that's unexpected.

logDebug("BEGIN _________________________________")

logDebug("In nomis-alerts-expand-or-describe.applescript")

tell application "System Events"
    set _w to null
    try
        set _w to window "Notification Center" ¬
                  of application process "NotificationCenter"
    on error errMsg number errNum
        tell me to logInfo(_msg_when_no_notifications)
    end try
    if _w is not null then
        try

            -- Find the topmost item.
            set _topmost_y_so_far          to null
            set _topmost_item_group_so_far to null
            repeat with _group1 in groups of _w
                tell me to logDebug("1-top-level " & description of _group1)
                repeat with _item_group ¬
                in groups of UI element 1 of scroll area 1 of _group1
                    set [_x, _y] to position of _item_group
                    tell me to logDebug("  2-item: " ¬
                                   & "y = " & _y & " " ¬
                                   & description of _item_group ¬
                                   & " " ¬
                                   & the value of static text 1 of _item_group ¬
                                   & " " ¬
                                   & the value of static text 2 of _item_group)
                    if _topmost_y_so_far = null ¬
                    or _y < _topmost_y_so_far then
                        set _topmost_y_so_far to _y
                        set _topmost_item_group_so_far to _item_group
                    end if
                end repeat
            end repeat

            -- Log details of topmost item.
            tell me to logDebug("  --------")
            set _item_group to _topmost_item_group_so_far
            set [_x, _y] to position of _item_group
            tell me to logDebug("  2-topmost-item: " ¬
                           & "y = " & _y & " " ¬
                           & description of _item_group ¬
                           & " " ¬
                           & the value of static text 1 of _item_group ¬
                           & " " ¬
                           & the value of static text 2 of _item_group)

            -- Find the relevant actions of the topmost item.
            set _item_descs to {}
            set _actions to actions of _topmost_item_group_so_far
            set _close_action to null
            set _press_action to null
            set _clear_all_action to null
            repeat with _action in _actions
                set _item_desc to description of _action
                set end of _item_descs to _item_desc
                tell me to logDebug("    3-action: " & _item_desc)
                if _item_desc = _press_desc then
                    set _press_action to _action
                else if _item_desc = _close_desc then
                    set _close_action to _action
                else if _item_desc = _clear_all_desc then
                    set _clear_all_action to _action
                end if
            end repeat

            -- Decide what to do.
            set _option_down_p to option_down of my getModifierKeys()
            set _action_to_perform to null
            if my isIn(_item_descs, _item_descs_s_for_collapsed_apps) then
                if _option_down_p then
                    set _action_to_perform to _clear_all_action
                else
                    set _action_to_perform to _press_action
                end if
            else if _close_action is not null then
                set _action_to_perform to _close_action
            end if

            -- Do what we've decided to do.
            if _action_to_perform = null then
                tell me to logInfo("COULD NOT WORK OUT WHAT TO DO")
            else
                perform _action_to_perform
                -- Passing `_action_to_perform` as a parameter causes a weird
                -- error, so grab its description and pass that -- that works.
                set _desc to description of _action_to_perform
                set _msg to my messageForAction(_desc, _option_down_p)
                tell me to logInfo(_msg)
            end if
        on error errMsg number errNum
            display dialog errMsg buttons {"OK"} -- Do this first because the following is flakey.
            tell me to logInfo("ERROR: Details follow...")
            set _msg to do shell script ¬
                "echo " & quoted form of errMsg & ¬
                " | nomis-remove-quotes-and-newlines"
            tell me to logInfo("ERROR: " & _msg)
        end try
    end if
end tell

logDebug("END _________________________________")
