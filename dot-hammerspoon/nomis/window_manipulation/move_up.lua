require("nomis.window_manipulation.bounds")
require("nomis.window_manipulation.common")

function nomisWindowMoveUp ()
   local win = hs.window.focusedWindow()
   local winFrame = win:frame()
   local screenFrame = win:screen():frame()

   local cellYs = cellYs()

   local i = nRows
   while i > 0 and cellYs[i] >= winFrame.y1 - fudgeFactor do
      i = i - 1
   end

   local newY1 = cellYs[i]

   setBounds(winFrame.x1,
             newY1,
             winFrame.x2,
             newY1 + winFrame.h)
end
