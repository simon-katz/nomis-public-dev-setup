-- `Command-backtick` is broken in macOS Ventura, for some apps at least.

-- Our Hammerspoon fix in `nomisCycleAppWindowsImpl` only works for some apps
-- (maybe because of bugs in experimental funcionality in Hammerspoon, maybe
-- because of something else), so we only use our fix for apps where
-- `Command-backtick` is broken.

-- Inspiration is taken from
-- https://github.com/Hammerspoon/hammerspoon/issues/856#issuecomment-209542078
-- but this code here recomputes the window list each time, and works when
-- windows are created or deleted.

-- If we ever come to fix bugs here, maybe look at the abandoned
-- `fix-window-cycling-v2` Git branch.

-- TODO: Maybe reply at the above link giving this improved implementation.
--       (and remember you have `nomisIndexof` and `nomisContains`).

local namesOfBrokenapps = {"Google Chrome",
                           "iTerm2"}

local function nomisCycleAppWindowsImpl(isForward, currentApp)
   local currentWindow = hs.window.focusedWindow()
   if currentWindow == nil then
      nomisMessage("No current window")
   else
      local wf = hs.window.filter.new(function(win)
            return (win:isStandard()
                    and win:application() == currentApp
                    -- and win:screen() == currentWindow:screen()
            )
      end)
      local windows = wf:getWindows(hs.window.filter.sortByCreatedLast)
      if #windows == 0 then
         nomisMessage("ERROR: nomisCycleAppWindowsHelper: Found zero windows")
      elseif #windows == 1 then
         nomisMessage("There are no other windows")
      else
         local currentIndex = nomisIndexof(windows, currentWindow)
         local delta = isForward and 1 or -1
         local newIndex = (currentIndex - 1 + delta) % #windows + 1
         local newWindow = windows[newIndex]
         newWindow:focus()
      end
   end
end

local function nomisCycleAppWindowsHelper(isForward)
   local currentApp = hs.application.frontmostApplication()
   local currentAppName = currentApp:name()
   local useHammerspoon = nomisContains(namesOfBrokenapps, currentAppName)
   if useHammerspoon then
      nomisLog("nomisCycleAppWindowsHelper: Using Hammerspoon for " .. currentAppName)
      nomisCycleAppWindowsImpl(isForward, currentApp)
   else
      nomisLog("nomisCycleAppWindowsHelper: Using Command-backtick for " .. currentAppName)
      local modifiers = isForward and {"cmd"} or {"shift", "cmd"}
      -- Send the keystroke directly to the app. If we don't do that,
      -- perhaps we'd reinvoke skhd and then this function, and so get
      -- into an infinite recursion.
      hs.eventtap.keyStroke(modifiers, "`", nil, currentApp)
   end
end

function nomisCycleAppWindows()
   nomisCycleAppWindowsHelper(true)
end

function nomisCycleAppWindowsBackwards()
   nomisCycleAppWindowsHelper(false)
end
