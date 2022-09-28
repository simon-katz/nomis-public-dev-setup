require("nomis.window_manipulation.bounds")
require("nomis.window_manipulation.common")

function nomisWindowMoveRight ()
   local win = hs.window.focusedWindow()
   local winFrame = win:frame()
   local screenFrame = win:screen():frame()

   local cellXs = cellXs()

   local i = 1
   while i < nCols and cellXs[i] <= winFrame.x1 + fudgeFactor do
      i = i + 1
   end

   local newX1
   if cellXs[i] + winFrame.w <= screenFrame.x2
   then
      newX1 = cellXs[i]
   else
      newX1 = screenFrame.x2 - winFrame.w
   end

   setBounds(newX1,
             winFrame.y1,
             newX1 + winFrame.w,
             winFrame.y2)
end
