require("nomis.window_manipulation.common")

--------------------------------------------------------------------------------

nomisBoundsAlertFeedbackDuration = 0.7

--------------------------------------------------------------------------------

local getBoundsOfDesktopScript = [[
tell application "Finder"
    set b to bounds of window of desktop
end tell
return b
]]

function getBoundsOfDesktop ()
   success, output, rawOutput =
      hs.osascript.applescript(getBoundsOfDesktopScript)
   return output[1], output[2], output[3], output[4]
end

--------------------------------------------------------------------------------
---- getBounds ----

local getBoundsScript = [[
-- Use System Events (Accessibility API) rather than talking directly to the
-- application. Talking directly can cause a hang if the app doesn't support
-- the AppleScript `bounds` property (e.g. ForkLift).
tell application "System Events"
    set curProc to first application process whose frontmost is true
    tell front window of curProc
        set pos  to position
        set sz   to size
    end tell
end tell

set x1 to item 1 of pos
set y1 to item 2 of pos
set x2 to x1 + item 1 of sz
set y2 to y1 + item 2 of sz
return {x1, y1, x2, y2}
]]

function getBounds ()
   success, output, rawOutput = hs.osascript.applescript(getBoundsScript)
   return output[1], output[2], output[3], output[4]
end

--------------------------------------------------------------------------------
---- setBounds ----

local function indexOf(array, value)
   -- Return the first index with the given value (or nil if not found).
   -- Based on https://stackoverflow.com/a/69651531
   if value == array[0] then return 0 end
   for i, v in ipairs(array) do
      if v - fudgeFactor < value and v + fudgeFactor > value then
         return i
      end
   end
   return nil
end

local setBoundsScriptFormatString = [[
-- Use System Events (Accessibility API) rather than talking directly to the
-- application. Talking directly can cause a hang if the app doesn't support
-- the AppleScript `bounds` property (e.g. ForkLift).
tell application "System Events"
    set curProc to first application process whose frontmost is true
    set procName to name of curProc
    if procName contains "Emacs" then
        -- We choose not to deal with Emacs, for two reasons:
        -- - We generally have multiple instances of Emacs and we don't know
        --   how to deal with that in AppleScript.
        -- - Some AppleScript doesn't seem to work with Emacs.
        error "Choosing not to run AppleScript on Emacs"
    end if
    tell front window of curProc
        set position to {%s, %s}
        set size     to {%s, %s}
    end tell
end tell
]]

local function setBoundsScript (x1, y1, x2, y2)
   -- System Events uses position {x, y} and size {w, h}, not bounds {x1,y1,x2,y2}.
   return string.format(setBoundsScriptFormatString, x1, y1, x2 - x1, y2 - y1)
end

function buggyHammerspoonSetBounds (x1, y1, x2, y2)
   -- This doesn't work reliablly (bugs!).
   local win = hs.window.focusedWindow()
   local winFrame = win:frame()
   winFrame.x1 = x1
   winFrame.y1 = y1
   winFrame.x2 = x2
   winFrame.y2 = y2
   win:setFrame(winFrame)
end

local mostRecentSetBoundsInfoAlertUuid = nil

function displayBoundsFeedbackHelper (x1, y1, x2, y2, extraInfo, feedbackDuration)
   local cellXs = cellXs()
   local cellYs = cellYs()
   local cellWidth = cellWidth()
   local cellHeight = cellHeight()

   local x1CellNo = indexOf(cellXs, x1)
   local y1CellNo = indexOf(cellYs, y1)
   local x2CellNo = indexOf(cellXs, x2)
   local y2CellNo = indexOf(cellYs, y2)

   if x1CellNo == nil then x1CellNo = (x1 - cellXs[0]) / cellWidth end
   if y1CellNo == nil then y1CellNo = (y1 - cellYs[0]) / cellHeight end
   if x2CellNo == nil then x2CellNo = (x2 - cellXs[0]) / cellWidth  end
   if y2CellNo == nil then y2CellNo = (y2 - cellYs[0]) / cellHeight end
   local xNCells  = x2CellNo - x1CellNo
   local yNCells  = y2CellNo - y1CellNo

   local formatString
   if x1CellNo ~= math.floor(x1CellNo) or
      y1CellNo ~= math.floor(y1CellNo) or
      x2CellNo ~= math.floor(x2CellNo) or
      y2CellNo ~= math.floor(y2CellNo)
   then
      formatString = "%5.2f"
   else
      formatString = "%2d"
   end

   x1CellNo = string.format(formatString, x1CellNo)
   y1CellNo = string.format(formatString, y1CellNo)
   x2CellNo = string.format(formatString, x2CellNo)
   y2CellNo = string.format(formatString, y2CellNo)
   xNCells  = string.format(formatString, xNCells)
   yNCells  = string.format(formatString, yNCells)

   if mostRecentSetBoundsInfoAlertUuid ~= nil then
      hs.alert.closeSpecific(mostRecentSetBoundsInfoAlertUuid)
   end

   local hackedExtraInfo
   if extraInfo then
      hackedExtraInfo = "\n\n" .. extraInfo
   else
      hackedExtraInfo = ""
   end

   mostRecentSetBoundsInfoAlertUuid =
      nomisMessage("     Top left:"
                   .. " " .. x1CellNo
                   .. "," .. y1CellNo
                   .. " \n\n         Size:"
                   .. " "  .. xNCells
                   .. ","  .. yNCells
                   .. " \n\n Bottom right:"
                   .. " "  .. x2CellNo
                   .. ","  .. y2CellNo
                   -- .. " \n\n Coordinates:"
                   -- .. " ("   .. x1
                   -- .. ","    .. y1
                   -- .. "), (" .. x2
                   -- .. ","    .. y2
                   -- .. ")"
                   .. " "
                   .. hackedExtraInfo ,
                   {textFont = "courier"},
                   feedbackDuration)
end

function displayBoundsFeedbackWithNormalFeedbackDuration (x1, y1, x2, y2, extraInfo)
   displayBoundsFeedbackHelper(x1, y1, x2, y2, extraInfo)
end

function displayBoundsFeedbackWithshortFeedbackDuration (x1, y1, x2, y2, extraInfo)
   displayBoundsFeedbackHelper(x1, y1, x2, y2, extraInfo, nomisBoundsAlertFeedbackDuration)
end

local giveBoundsFeedback = true

function setBounds (x1, y1, x2, y2)
   x1 = math.floor(x1 + 0.5)
   y1 = math.floor(y1 + 0.5)
   x2 = math.floor(x2 + 0.5)
   y2 = math.floor(y2 + 0.5)
   if giveBoundsFeedback then
      displayBoundsFeedbackWithshortFeedbackDuration(x1, y1, x2, y2)
   end
   -- `buggyHammerspoonSetBounds` doesn't work reliablly, so use Applescript
   -- instead...
   local success, output, rawOutput =
      hs.osascript.applescript(setBoundsScript(x1, y1, x2, y2))
   if not success
   then
      local appName = hs.window.focusedWindow():application():name()
      local msg = string.format("setBounds: AppleScript failed. Using Hammerspoon functionality instead. Application: %s.",
                                appName)
      nomisLog(msg)
      buggyHammerspoonSetBounds(x1, y1, x2, y2)
   end
end
