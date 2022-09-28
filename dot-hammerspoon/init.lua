--------------------------------------------------------------------------------
---- A useful message ----

hs.alert.show("Reloading Hammerspoon config")

--------------------------------------------------------------------------------
---- Global log file ----

function nomisLog (...)
   local dateTime = os.date("%Y-%m-%d %H:%M:%S,000")
   local logfile = io.open("logs/hammerspoon.log", "a+")
   logfile:write(dateTime, " [] DEBUG x - ") -- a standard format
   local nn = select('#',...)
   for n=1,select('#',...) do
      local v = select(n,...)
      logfile:write(tostring(v))
      -- if n ~= nn then logfile:write(" ") end
   end
   logfile:write("\n")
   logfile:close()
end

--------------------------------------------------------------------------------
---- Auto-reload config ----

-- Based on https://www.hammerspoon.org/go/#fancyreload

function reloadConfig(files)
   doReload = false
   for _,file in pairs(files) do
      local fileName = file:match("^.+/(.+)$")
      if (fileName:sub(1, 2) ~= ".#") and (fileName:sub(-4) == ".lua") then
         doReload = true
      end
   end
   if doReload then
      hs.reload()
   else
   end
end

local autoReloadWatcher
   = hs.pathwatcher.new(os.getenv("HOME") .. "/.hammerspoon/",
                        reloadConfig)

autoReloadWatcher:start()

--------------------------------------------------------------------------------

require("hs.ipc")
require("nomis/bits_and_bobs")
require("nomis/totalspaces3")
-- require("nomis/z_obsolete/window_manipulation_using_hs_grid")
-- require("nomis/z_obsolete/window_manipulation_using_applescripts")
require("nomis/window_manipulation")
