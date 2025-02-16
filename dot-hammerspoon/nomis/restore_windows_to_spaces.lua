--------------------------------------------------------------------------------
---- nomisNoteWindowsSpaces ----

-- Refs:
--   hs.spaces.spacesForScreen (https://www.hammerspoon.org/docs/hs.spaces.html#spacesForScreen)
--   hs.spaces.windowsForSpace(spaceID) (https://www.hammerspoon.org/docs/hs.spaces.html#windowsForSpace)
--     - Shit: "only those window IDs that are present on the currently visible spaces will be finable"
--   hs.spaces.moveWindowToSpace (https://www.hammerspoon.org/docs/hs.spaces.html#moveWindowToSpace)

-- TODO: Do Chromw windows with multiple tabs reopen at the same tab?
-- TODO: Need to address the "Shit" note above. We'd have to make each space
--       visible in turn and get its windows. I'm giving up on this.
-- TODO: Maybe you can navigate from app to windows, and find which space each
--       window is on.
--       - hs.application:allWindows()
--         => A table of zero or more hs.window objects owned by the application.
--       - hs.spaces.windowSpaces(window)
--         Returns a table containing the space IDs for all spaces that the
--         specified window is on.

function nomisNoteWindowsSpaces ()
   nomisLog(">>>> nomisNoteWindowsSpaces")

   -- TODO: What's with the space numbers?

   -- TODO: See these errors in the Hammerspoon Console:
   -- 2025-02-16 20:02:19: 20:02:19 ERROR:   LuaSkin: hs.ipc:callback - error during callback for Hammerspoon: ...Hammerspoon.app/Contents/Resources/extensions/hs/ipc.lua:420: ipc port is no longer valid (early)
   -- stack traceback:
   -- 	[C]: in method 'sendMessage'
   -- 	...Hammerspoon.app/Contents/Resources/extensions/hs/ipc.lua:420: in function 'hs.libipc.__defaultHandler'

   local spaces = hs.spaces.spacesForScreen() -- TODO: Takes an optional arg.
   for spaceNumber, spaceId in ipairs(spaces) do
      windows = hs.spaces.windowsForSpace(spaceId)
      nomisLog("Space ", spaceNumber, " (spaceId ", spaceId, "):")
      if windows then
         for windowNumber, windowId in ipairs(windows) do
            local w = hs.window(windowId)
            if w then
               local app = w:application()
               if app:name() == "Google Chrome" then
                  nomisLog("    Window ", windowNumber, ": ", windowId)
                  nomisLog("        title = ", "\"", hs.window(windowId):title(), "\"")
                  -- nomisLog("        title = ", "\"", hs.window.find(windowId):title(), "\"")
                  -- nomisLog("        title = ", "\"", hs.window.get(windowId):title(), "\"")
               end
            end
         end
      end
   end
   nomisLog("<<<< nomisNoteWindowsSpaces")
end
