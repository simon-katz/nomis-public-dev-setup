require("nomis.window_manipulation.bounds")
require("nomis.window_manipulation.common")

function nomisWindowDecWidth ()
   local win = hs.window.focusedWindow()
   local winFrame = win:frame()
   local screenFrame = win:screen():frame()

   local cellXs = cellXs()
   local cellWidth = cellWidth()

   local i = nCols
   while i > 0 and cellXs[i] >= winFrame.x2 - fudgeFactor do
      i = i - 1
   end

   local newX2
   if cellXs[i] >= winFrame.x1 + cellWidth
   then
      newX2 = cellXs[i]
   else
      newX2 = winFrame.x1 + cellWidth
   end

   setBounds(winFrame.x1,
             winFrame.y1,
             newX2,
             winFrame.y2)
end
