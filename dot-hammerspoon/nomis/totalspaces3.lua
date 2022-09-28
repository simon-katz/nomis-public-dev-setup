--------------------------------------------------------------------------------
---- Helpers ----

local function nomisRestartTotalSpacesHelper ()
   os.execute("~/nomis-bin/nomis-restart-totalspaces3")
end

--------------------------------------------------------------------------------
---- Restart TotalSpaces3 ----

function nomisRestartTotalSpaces ()
   nomisRestartTotalSpacesHelper()
   nomisMessage("Restarting TotalSpaces3")
end

--------------------------------------------------------------------------------
---- Restart TotalSpaces3 after waking ----

-- Copied from https://discuss.binaryage.com/t/can-we-help-test-total-spaces-3-if-we-have-apple-silicon/8199/179

function nomisRestartTotalSpacesAfterWaking ()
   nomisRestartTotalSpacesHelper()
   nomisMessage("Restarting TotalSpaces3 after waking")
end

local wakeUpWatcher = hs.caffeinate.watcher.new(function(eventType)
      if eventType == hs.caffeinate.watcher.systemDidWake then
         nomisRestartTotalSpacesAfterWaking()
      end
end)

wakeUpWatcher:start()
