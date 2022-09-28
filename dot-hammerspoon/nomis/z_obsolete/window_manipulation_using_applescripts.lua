require("nomis.z_obsolete.nomis_applescripts")

nomisMessage("Whoops! Loading obsolete window-manipulation-using-applescripts")

function nomisWindowMoveLeft () runNomisApplescriptFile("window-move-left")       end
function nomisWindowMoveRight() runNomisApplescriptFile("window-move-right")      end
function nomisWindowMoveUp   () runNomisApplescriptFile("window-move-up")         end
function nomisWindowMoveDown () runNomisApplescriptFile("window-move-down")       end
function nomisWindowDecWidth () runNomisApplescriptFile("window-width-decrease")  end
function nomisWindowIncWidth () runNomisApplescriptFile("window-width-increase")  end
function nomisWindowDecHeight() runNomisApplescriptFile("window-height-decrease") end
function nomisWindowIncHeight() runNomisApplescriptFile("window-height-increase") end
