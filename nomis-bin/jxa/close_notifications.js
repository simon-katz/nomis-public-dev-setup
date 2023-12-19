// From https://apple.stackexchange.com/questions/155670/macos-keyboard-shortcut-to-dismiss-notifications

// Modified from
// https://gist.github.com/lancethomps/a5ac103f334b171f70ce2ff983220b4f
"use strict";

function run() {
    var app = Application.currentApplication();
    app.includeStandardAdditions = true;


    const SystemEvents = Application("System Events");
    const NotificationCenter = SystemEvents.processes.byName("NotificationCenter");
    const notificationGroups = () => {
        const windows = NotificationCenter.windows;

        // app.displayDialog("No of windows = " + windows.length);

        return windows.length === 0
            ? []
            : windows.at(0).groups.at(0).scrollAreas.at(0).uiElements.at(0).groups();
    };

    const findCloseAction = (group) => {
        const [closeAllAction,
               closeAction] = group.actions().reduce(
                   (matches, action) => {
                       switch (action.description()) {
                       case "Clear All":
                           return [action, matches[1]];
                       case "Close":
                           return [matches[0], action];
                       default:
                           return matches;
                       }
                   },
                   [null, null]
               );
        return closeAllAction ?? closeAction;
    };

    const actions = notificationGroups().map(findCloseAction);

    // app.displayDialog("No of actions = " + actions.length);


    for (const action of actions) {
        action?.perform();
        // throw new Error('xxxx');
    }
    // const firstAction = actions.first();
    // firstAction.perform();

    // var app = Application.currentApplication();
    // app.displayDialog(app.name());
    var app = Application('/Applications/Emacs-28-2.app');
    // var app = Application(oProcess.name()); // Application("System Events");
    app.includeStandardAdditions = true;

    app.displayNotification("js aaaa", {
        withTitle: "js title",
        subtitle: "js subtitle",
        soundName: "Frog"
    });
}
