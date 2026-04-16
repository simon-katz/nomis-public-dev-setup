use scripting additions

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
-- Messages

set _msg_when_no_notifications  to "There are no notifications"
set _msg_when_items_not_found   to "Could not find notifications in UI (structure may have changed)"
set _expand_msg                 to "Expanded notifications"
set _collapse_msg               to "Collapsed notifications"
set _dismiss_msg                to "Dismissed notification"

set _press_desc     to "press"
set _close_desc     to "Close"
set _clear_all_desc to "Clear All"

--------------------------------------------------------------------------------
-- Main

-- The approach:
--   - Find the top-most item (using y coordinate).
--   - Single notification (container has no child groups):
--     Do "Close" to dismiss it.
--   - Stacked (topmost item has "Clear All", no "Close"):
--     Do "press" to expand.
--   - Expanded (topmost item has "Close", container has child groups):
--     Click button 1 of the container to collapse back to a stack.
--     (The individual × close button only appears on hover, so dismissing
--     one at a time via accessibility is not possible in Tahoe.)

logDebug("BEGIN _________________________________")

logDebug("In nomis-alerts-expand-or-describe.applescript")

tell application "System Events"
    set _w to null
    try
        -- Use window 1 rather than window "Notification Center" to avoid
        -- locale issues (e.g. "Notification Centre" in British English).
        set _w to window 1 ¬
                  of application process "NotificationCenter"
    on error errMsg number errNum
        tell me to logInfo(_msg_when_no_notifications)
    end try
    if _w is not null then
        try

            -- Find the topmost item.
            set _topmost_y_so_far          to null
            set _topmost_item_group_so_far to null
            set _topmost_container         to null
            set _topmost_is_single         to false
            repeat with _group1 in groups of _w
                tell me to logDebug("1-top-level " & description of _group1)
                -- In Tahoe the scroll area is one level deeper than before:
                --   old: scroll area 1 of _group1
                --   new: scroll area 1 of group 1 of _group1
                -- For stacked/expanded notifications, items are child groups
                -- of the container. For a single notification, the container
                -- itself is the item (no child groups).
                set _container to group 1 of scroll area 1 of group 1 of _group1
                set _child_groups to groups of _container
                set _is_single to (count of _child_groups) = 0
                if _is_single then
                    set _candidates to {_container}
                else
                    set _candidates to _child_groups
                end if
                repeat with _item_group in _candidates
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
                        set _topmost_y_so_far          to _y
                        set _topmost_item_group_so_far to _item_group
                        set _topmost_container         to _container
                        set _topmost_is_single         to _is_single
                    end if
                end repeat
            end repeat

            if _topmost_item_group_so_far is null then
                -- Items not found — the UI tree structure may have changed.
                tell me to logInfo(_msg_when_items_not_found)
            else

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

                set _actions to actions of _topmost_item_group_so_far
                set _close_action     to null
                set _press_action     to null
                set _clear_all_action to null
                repeat with _action in _actions
                    set _item_desc to description of _action
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
                set _action_to_perform to null
                set _do_collapse       to false
                set _feedback_msg      to null
                if _topmost_is_single then
                    -- Single notification: dismiss it.
                    set _action_to_perform to _close_action
                    set _feedback_msg      to _dismiss_msg
                else if _clear_all_action is not null and _close_action is null then
                    -- Stacked: expand.
                    set _action_to_perform to _press_action
                    set _feedback_msg      to _expand_msg
                else if _close_action is not null then
                    -- Expanded: collapse back to a stack.
                    -- The individual × close button only appears on hover so
                    -- we cannot dismiss one at a time via accessibility.
                    set _do_collapse  to true
                    set _feedback_msg to _collapse_msg
                end if

                -- Do what we've decided to do.
                if not _do_collapse and _action_to_perform = null then
                    tell me to logInfo("COULD NOT WORK OUT WHAT TO DO")
                else
                    if _do_collapse then
                        click button 1 of _topmost_container
                    else
                        -- Get the description before performing: the UI reference
                        -- becomes stale after the notification changes or disappears.
                        set _desc to description of _action_to_perform
                        perform _action_to_perform
                    end if
                    tell me to logInfo(_feedback_msg)
                end if

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
