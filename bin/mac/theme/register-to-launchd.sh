#!/usr/bin/env bash

cd "$(dirname "$0")"

# register to launchctl
launchctl load github.ryuheechul.try-switching.plist


# other useful commands

##unload
#launchctl unload github.ryuheechul.try-switching.plist

## test
#launchctl start github.ryuheechul.try-switching


## about launchd
# https://nathangrigg.com/2012/07/schedule-jobs-using-launchd#permissions
# https://www.launchd.info/
