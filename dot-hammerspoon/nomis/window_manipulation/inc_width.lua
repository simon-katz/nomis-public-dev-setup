require("nomis.window_manipulation.bounds")
require("nomis.window_manipulation.common")

function nomisWindowIncWidth ()
   local win = hs.window.focusedWindow()
   local winFrame = win:frame()
   local screenFrame = win:screen():frame()

   local cellXs = cellXs()

   local i = 1
   while i < nCols and cellXs[i] <= winFrame.x2 + fudgeFactor do
      i = i + 1
   end

   local newX2
   if cellXs[i] <= screenFrame.x2
   then
      newX2 = cellXs[i]
   else
      newX2 = screenFrame.x2
   end

   setBounds(winFrame.x1,
             winFrame.y1,
             newX2,
             winFrame.y2)
end
