nCols = 16
nRows = 16

fudgeFactor = 2 -- Amount of x, y difference to be disregarded.

function cellWidth ()
   local win = hs.window.focusedWindow()
   local winFrame = win:frame()
   local screenFrame = win:screen():frame()
   return screenFrame.w / nCols
end

function cellHeight ()
   local win = hs.window.focusedWindow()
   local winFrame = win:frame()
   local screenFrame = win:screen():frame()
   return screenFrame.h / nRows
end

function cellXs ()
   local win = hs.window.focusedWindow()
   local winFrame = win:frame()
   local screenFrame = win:screen():frame()
   values = {}
   for i = 0, nCols do
      values[i] = math.floor(screenFrame.x1 + (i * screenFrame.w) / nCols)
   end
   return values
end

function cellYs ()
   local win = hs.window.focusedWindow()
   local winFrame = win:frame()
   local screenFrame = win:screen():frame()
   values = {}
   for i = 0, nRows do
      values[i] = math.floor(screenFrame.y1 + (i * screenFrame.h) / nRows)
   end
   return values
end
