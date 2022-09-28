local nomisApplescriptsDir = "/Users/simonkatz/development-100/repositories/nomis/dev-setup/nomis-public-dev-setup/applescripts-etc"

local function nomisApplescriptFile (s)
   return nomisApplescriptsDir .. "/" .. s .. ".applescript"
end

function runNomisApplescriptFile (s)
   return hs.osascript.applescriptFromFile(nomisApplescriptFile(s))
end
