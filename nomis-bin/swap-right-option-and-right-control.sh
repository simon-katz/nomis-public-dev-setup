#!/usr/bin/env bash

#### Swap right-option and right-control on a Mac.
#### See https://developer.apple.com/library/content/technotes/tn2450/_index.html
#### See https://apple.stackexchange.com/questions/283252/how-do-i-remap-a-key-in-macos-sierra-e-g-right-alt-to-right-control

#### This change is lost when rebooting, hence this startup script.

hidutil property --set '{"UserKeyMapping":
    [{"HIDKeyboardModifierMappingSrc":0x7000000e4,
      "HIDKeyboardModifierMappingDst":0x7000000e6},
     {"HIDKeyboardModifierMappingSrc":0x7000000e6,
      "HIDKeyboardModifierMappingDst":0x7000000e4}]
}'


#### Here'e the code for undoing things.
#### (But note the change is list on a reboot, so this may not be needed.)
# hidutil property --set '{"UserKeyMapping":
#     [{"HIDKeyboardModifierMappingSrc":0x7000000e4,
#       "HIDKeyboardModifierMappingDst":0x7000000e4},
#      {"HIDKeyboardModifierMappingSrc":0x7000000e6,
#       "HIDKeyboardModifierMappingDst":0x7000000e6}]
# }'
