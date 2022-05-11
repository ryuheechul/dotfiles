#!/usr/bin/env bash

# this script can be called from an Automator app to mitigate https://github.com/nix-community/home-manager/issues/1341
# and put the (Automator) app in either /Applications or ~/Applications so either Spotlight or Alfred can open (or activate the (Emacs) app quickly
# Also avoid naming the Automator app "Emacs.app" to prevent collision with "~/.nix-profile/Applications/Emacs.app" on `"Emacs" to activate` Applescript side - For example, name it to "OpenEmacs.app"

osascript -e 'if application "Emacs" is not running then
	error "emacs is not running"
end if' 2> /dev/null && {
	osascript -e 'tell application "Emacs" to activate'
} || {
	open -a ~/.nix-profile/Applications/Emacs.app
}
