require("nomis.window_manipulation.bounds")
require("nomis.window_manipulation.common")

function nomisWindowIncHeight ()
   local win = hs.window.focusedWindow()
   local winFrame = win:frame()
   local screenFrame = win:screen():frame()

   local cellYs = cellYs()

   local i = 1
   while i < nRows and cellYs[i] <= winFrame.y2 + fudgeFactor do
      i = i + 1
   end

   local newY2
   if cellYs[i] <= screenFrame.y2
   then
      newY2 = cellYs[i]
   else
      newY2 = screenFrame.y2
   end

   setBounds(winFrame.x1,
             winFrame.y1,
             winFrame.x2,
             newY2)
end
