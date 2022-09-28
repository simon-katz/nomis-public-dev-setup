---- See https://www.hammerspoon.org/docs/hs.grid.html

-- I don't like this.
-- - There's usually a margin at the right bottom of the screen.
-- - When resizing, it does silly things: /eg/ for `resizeWindowWider`, if the
--   window hits the right edge of the screen and is asked to become wider, its
--   left edge will shift.

hs.grid.setMargins({0, 0})
hs.grid.setGrid('16x16')

--------------------------------------------------------------------------------

function nomisWindowMoveLeft  () hs.grid.pushWindowLeft(hs.window.focusedWindow())  end
function nomisWindowMoveRight () hs.grid.pushWindowRight(hs.window.focusedWindow()) end
function nomisWindowMoveUp    () hs.grid.pushWindowUp(hs.window.focusedWindow())    end
function nomisWindowMoveDown  () hs.grid.pushWindowDown(hs.window.focusedWindow())  end

--------------------------------------------------------------------------------

function nomisWindowDecWidth  () hs.grid.resizeWindowThinner(hs.window.focusedWindow()) end
function nomisWindowIncWidth  () hs.grid.resizeWindowWider  (hs.window.focusedWindow()) end
function nomisWindowDecHeight () hs.grid.resizeWindowShorter(hs.window.focusedWindow()) end
function nomisWindowIncHeight () hs.grid.resizeWindowTaller (hs.window.focusedWindow()) end
