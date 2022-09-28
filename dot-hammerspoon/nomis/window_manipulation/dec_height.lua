require("nomis.window_manipulation.bounds")
require("nomis.window_manipulation.common")

function nomisWindowDecHeight ()
   local win = hs.window.focusedWindow()
   local winFrame = win:frame()
   local screenFrame = win:screen():frame()

   local cellYs = cellYs()
   local cellHeight = cellHeight()

   local i = nRows
   while i > 0 and cellYs[i] >= winFrame.y2 - fudgeFactor do
      i = i - 1
   end

   local newY2
   if cellYs[i] >= winFrame.y1 + cellHeight
   then
      newY2 = cellYs[i]
   else
      newY2 = winFrame.y1 + cellHeight
   end

   setBounds(winFrame.x1,
             winFrame.y1,
             winFrame.x2,
             newY2)
end
