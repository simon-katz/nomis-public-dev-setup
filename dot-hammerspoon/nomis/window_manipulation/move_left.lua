require("nomis.window_manipulation.bounds")
require("nomis.window_manipulation.common")

function nomisWindowMoveLeft ()
   local win = hs.window.focusedWindow()
   local winFrame = win:frame()
   local screenFrame = win:screen():frame()

   local cellXs = cellXs()

   local i = nCols
   while i > 0 and cellXs[i] >= winFrame.x1 - fudgeFactor do
      i = i - 1
   end

   local newX1 = cellXs[i]

   setBounds(newX1,
             winFrame.y1,
             newX1 + winFrame.w,
             winFrame.y2)
end
