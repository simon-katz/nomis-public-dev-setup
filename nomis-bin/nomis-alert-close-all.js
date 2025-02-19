// Copied from Alfred's BannerBeGone workflow.

// Notification Center can be disrupted by the simplest of events.
// Clearing one notification or having an external screen
// can alter its behaviour in tiny ways which break notification clearing methods.
// Notification Center's structure keeps changing in minor macOS releases
// so there is no point chasing alert reference paths.
// The only sane way to detect banners and alerts is to look for specific known subroles:
//   AXNotificationCenterAlert
//   AXNotificationCenterAlertStack
//   AXNotificationCenterBanner
//   AXNotificationCenterBannerStack
// Similarly, looking for "Close" and "Clear" actions by name is suboptimal
// as every written language has a different word.
// They have consistently been placed as the last action, so that's what we use.
// Due to the aforementioned awkward behaviours,
// and because Notification Center's contents change when clearing each notification,
// it is impractical to get a full array of contents and act on them one by one.
// Some approaches work better than others but have large speed trade-offs
// that may leave the user thinking nothing is happening.
// This solution keeps requesting and clearing the first alert in sequence.
// It may not be the fastest in raw terms but it's immediatelly clear it's working.
// Because Notification Center is insane, many solutions may work most of the way
// but leave the last banner to be cleared.
// This is why we also add a number of retries until we're reasonably sure it's done.

let remainingTries = 5

while (true) {
  try {
    Application("System Events")
      .applicationProcesses.byName("NotificationCenter")
      .windows[0]
      .entireContents()
      .find(item => item.subrole()?.startsWith("AXNotificationCenter"))
      .actions()
      .slice(-1)[0]
      .perform()
  } catch {
    remainingTries--
    if (remainingTries < 1) break
  }
}
