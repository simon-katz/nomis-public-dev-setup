require("nomis.window_manipulation.bounds")
require("nomis.window_manipulation.common")

function nomisWindowMoveDown ()
   local win = hs.window.focusedWindow()
   local winFrame = win:frame()
   local screenFrame = win:screen():frame()

   local cellYs = cellYs()

   local i = 1
   while i < nRows and cellYs[i] <= winFrame.y1 + fudgeFactor do
      i = i + 1
   end

   local newY1
   if cellYs[i] + winFrame.h <= screenFrame.y2
   then
      newY1 = cellYs[i]
   else
      newY1 = screenFrame.y2 - winFrame.h
   end

   setBounds(winFrame.x1,
             newY1,
             winFrame.x2,
             newY1 + winFrame.h)
end
