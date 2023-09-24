-- For Command-` -- macOS's built-in version is broken for me in Ventura.

-- Inspiration taken from  https://github.com/Hammerspoon/hammerspoon/issues/856#issuecomment-209542078
-- This recomputes the window list each time, thereby avoiding a bug when
-- windows are created or deleted.

-- TODO: Maybe reply at the above link giving this improved implementation.
--       (and remember you have `nomisIndexof`).

local function nomisCycleAppWindowsHelper(delta)
   local currentWindow = hs.window.focusedWindow()
   if currentWindow == nil then
      nomisMessage("No current window")
   else
      local wf = hs.window.filter.new(function(win)
            return (win:isStandard() and
                    win:application() == currentWindow:application() and
                    win:screen() == currentWindow:screen())
      end)
      local windows = wf:getWindows(hs.window.filter.sortByCreatedLast)
      if #windows == 0 then
         nomisMessage("ERROR: nomisCycleAppWindowsHelper: Found zero windows")
      elseif #windows == 1 then
         nomisMessage("There are no other windows")
      else
         local currentIndex = nomisIndexof(windows, currentWindow)
         local newIndex = (currentIndex - 1 + delta) % #windows + 1
         local newWindow = windows[newIndex]
         newWindow:focus()
      end
   end
end

function nomisCycleAppWindows()
   nomisCycleAppWindowsHelper(1)
end

function nomisCycleAppWindowsBackwards()
   nomisCycleAppWindowsHelper(-1)
end
