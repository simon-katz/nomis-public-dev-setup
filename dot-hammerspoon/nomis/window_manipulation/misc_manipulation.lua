local nomisWindowToBounds = {}

function nomisWindowDisplayBounds (extraInfo)
   local win = hs.window.focusedWindow()
   local winFrame = win:frame()
   displayBoundsFeedbackWithNormalFeedbackDuration(winFrame.x1, winFrame.y1,
                                                   winFrame.x2, winFrame.y2,
                                                   extraInfo)
end

local function nomisNormalIsMaximized(win)
   local winFrame = win:frame()
   local screenFrame = win:screen():frame()
   return (winFrame.x1 == screenFrame.x1
           and winFrame.x2 == screenFrame.x2
           and winFrame.y1 == screenFrame.y1
           and winFrame.y2 == screenFrame.y2)
end

local nomisEmacsMaximizedFudgeFactor = 10

local function nomisEmacsIsMaximized(win)
   -- Emacs windows are screwy.
   local winFrame = win:frame()
   local screenFrame = win:screen():frame()
   local ff = nomisEmacsMaximizedFudgeFactor
   local isMaximized = (winFrame.x1 == screenFrame.x1
                        and winFrame.x2 >= screenFrame.x2 - ff
                        and winFrame.y1 == screenFrame.y1
                        and winFrame.y2 >= screenFrame.y2 - ff)
   return isMaximized
end

local function nomisIsMaximized (win)
   local appNameLowerCase = string.lower(win:application():name())
   local isEmacs = appNameLowerCase:find("emacs", 1, true) ~= nil
   if isEmacs then
      return nomisEmacsIsMaximized(win)
   else
      return nomisNormalIsMaximized(win)
   end
end

function nomisWindowMaximize ()
   local win = hs.window.focusedWindow()
   local winFrame = win:frame()
   if nomisIsMaximized(win) then
      displayBoundsFeedbackWithshortFeedbackDuration(winFrame.x1, winFrame.y1,
                                                     winFrame.x2, winFrame.y2,
                                                     "* Already maximized *")
   else
      nomisWindowToBounds[win:id()]
         = {winFrame.x1, winFrame.y1, winFrame.x2, winFrame.y2}
      local screenFrame = win:screen():frame()
      setBounds(screenFrame.x1, screenFrame.y1, screenFrame.x2, screenFrame.y2)
   end
end

function nomisWindowRestoreOldSize ()
   local win = hs.window.focusedWindow()
   local bounds = nomisWindowToBounds[win:id()]
   if bounds ~= nil then
      setBounds(bounds[1], bounds[2], bounds[3], bounds[4])
   else
      nomisMessage("I don't have the old size stored")
   end
end

function nomisWindowMoveToNextScreen ()
   local win = hs.window.focusedWindow()
   local currentScreen = win:screen()
   local nextScreen = currentScreen:next()
   if currentScreen == nextScreen then
      nomisMessage("Can't move to next screen because there is only one screen")
   else
      win:moveToScreen(nextScreen,
                       false,
                       false,
                       0)
   end
end
