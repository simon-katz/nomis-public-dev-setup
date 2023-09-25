-- For Command-` -- macOS's built-in version is broken for me in Ventura.

-- Inspiration taken from  https://github.com/Hammerspoon/hammerspoon/issues/856#issuecomment-209542078
-- This recomputes the window list each time, thereby avoiding a bug when
-- windows are created or deleted.

-- TODO: Maybe reply at the above link giving this improved implementation.
--       (and remember you have `nomisIndexof`).

-- local function newFilter()
--    return {override={visible=true,fullscreen=false,allowScreens='-1,0',currentSpace=true}
-- end

local function includeWindow(currentWindow, win)
   return (win:isStandard()
           and
           hs.spaces.windowSpaces(currentWindow)[1] == hs.spaces.windowSpaces(win)[1]
           and
           win:application() == currentWindow:application()
           -- and
           -- win:application():name() == currentWindow:application():name()
   )
end

-- hs.spaces.windowSpaces(window) -> table | nil, error
-- How can you get app from window?
-- How can you make the filter return all windows? (Want this because it gets
-- rid of some extra windows.)

-- local function nomisCycleAppWindowsDebug()
--    local currentWindow = hs.window.focusedWindow()
--    nomisLog("currentWindow                      = " .. tostring(currentWindow))
--    nomisLog("currentWindow:application()        = " .. tostring(currentWindow:application()))
--    nomisLog("currentWindow:application():name() = " .. tostring(currentWindow:application():name()))
--    nomisLog("__________")
--    local wCnt = 0
--    local wf2 = hs.window.filter.new(function(win)
--          wCnt = wCnt + 1
--          local isInclude = includeWindow(currentWindow, win)
--          if win:isStandard() then
--             nomisLog(wCnt .. " win                      = " .. tostring(win))
--             nomisLog(wCnt .. " win:application()        = " .. tostring(win:application()))
--             nomisLog(wCnt .. " win:application():name() = " .. tostring(win:application():name()))
--             nomisLog(wCnt .. " win:isStandard()         = " .. tostring(win:isStandard()))
--             nomisLog(wCnt .. " apps equal               = " .. tostring(win:application() == currentWindow:application()))
--             nomisLog(wCnt .. " app names equal          = " .. tostring(win:application():name() == currentWindow:application():name()))
--             nomisLog(wCnt .. " isInclude           = " .. tostring(isInclude))
--             nomisLog("__________")
--          end
--          return isInclude
--    end)
--    local windows2 = wf2:getWindows(hs.window.filter.sortByCreatedLast)
--    nomisLog("#windows2 = " .. #windows2)
-- end

local function nomisCycleAppWindowsHelper(delta)
   nomisLog("________________________________________")
   -- nomisCycleAppWindowsDebug()
   local currentWindow = hs.window.focusedWindow()
   if currentWindow == nil then
      nomisMessage("No current window")
   else
      nomisLog("currentWindow = " .. currentWindow:title())
      local app = currentWindow:application()
      local appName = app:name()
      nomisLog("app = " .. appName)
      local wf = hs.window.filter.new(function(win)
            return includeWindow(currentWindow, win)
            -- return true
      end)
      -- (true)


      -- local wf = hs.window.filter.defaultCurrentSpace

      local windows = wf:getWindows(hs.window.filter.sortByCreatedLast)
      local result = { hs.application.find(appName) }
      local appWindows = app:allWindows()
      nomisLog("#windows = " .. #windows)
      nomisLog("#result = " .. #result)
      -- table.sort(appWindows,
      --            (function (x, y)
      --                  return x.timeCreated > y.timeCreated
      --                  -- return true
      -- end))
      nomisLog("#appWindows = " .. #appWindows)
      for i, win in ipairs(
         windows
         -- result
         -- appWindows
      ) do
         nomisLog(tostring(i)
                  .. " "
                  .. tostring(win:isStandard())
                  .. " "
                  .. tostring(includeWindow(currentWindow, win))
                  .. " "
                  .. tostring(hs.spaces.windowSpaces(win))
                  .. " "
                  .. win:application():name()
                  .. " ---- "
                  .. win:title())
      end
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
   nomisLog("Done")
end

function nomisCycleAppWindows()
   nomisCycleAppWindowsHelper(1)
end

function nomisCycleAppWindowsBackwards()
   nomisCycleAppWindowsHelper(-1)
end
