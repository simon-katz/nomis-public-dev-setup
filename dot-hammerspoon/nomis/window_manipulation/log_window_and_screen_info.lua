require("nomis.window_manipulation.bounds")
require("nomis.window_manipulation.common")

function nomisWindowLogWindowAndScreenInfo ()
   hs.alert.show("Writing window info to log file...")

   local
      applescriptWinX1,
      applescriptWinY1,
      applescriptWinX2,
      applescriptWinY2 = getBounds()

   local win = hs.window.focusedWindow()
   local winFrame = win:frame()
   local screenFrame = win:screen():frame()

   nomisLog("________________________________________")
   nomisLog("cellWidth = ", cellWidth())
   nomisLog("cellHeight = ", cellHeight())
   nomisLog("cellXs = ", table.concat(cellXs(), ", ", 0))
   nomisLog("cellYs = ", table.concat(cellYs(), ", ", 0))
   nomisLog("screenFrame:              ",
            " x1=", screenFrame.x1,
            " y1=", screenFrame.y1,
            " x2=", screenFrame.x2,
            " y2=", screenFrame.y2,
            " w=",  screenFrame.w,
            " h=",  screenFrame.h)
   nomisLog("winFrame:                 ",
            " x1=", winFrame.x1,
            " y1=", winFrame.y1,
            " x2=", winFrame.x2,
            " y2=", winFrame.y2,
            " w=",  winFrame.w,
            " h=",  winFrame.h)
   nomisLog("AppleScript window bounds:",
            " x1=", applescriptWinX1,
            " y1=", applescriptWinY1,
            " x2=", applescriptWinX2,
            " y2=", applescriptWinY2,
            " w=",  applescriptWinX2 - applescriptWinX1,
            " h=",  applescriptWinY2 - applescriptWinY1)
   nomisLog("________________________________________")

   hs.alert.show("... written window info to log file")
end
