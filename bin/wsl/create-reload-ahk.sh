#!/usr/bin/env bash

WND_USERNAME="$(powershell.exe '$env:UserName' | sed 's/.$//')" # removing mysterious extra character at the end with sed

if [ -z "${WND_USERNAME}" ]; then
  echo 'Exiting because no $WND_USERNAME is set'
  echo "Try again like WND_USERNAME=my-wnd-user $(readlink -f $0)"
  exit 1
fi

target_dir="/mnt/c/Users/${WND_USERNAME}/AppData/Roaming/Microsoft/Windows/Start Menu/Programs/Startup"
target_path_for_win="$(wslpath -w "${target_dir}")\my-hotkeys.ahk.lnk"

to_link="$(wslpath -w "${my_dot_d}/autohotkey/my-hotkeys.ahk")"

cmd_to_run="powershell.exe 'New-Item -ItemType SymbolicLink -Path \"${target_path_for_win}\" -Target \"${to_link}\"'"

wudo="python3 ${HOME}/.wsl-sudo/wsl-sudo.py"

${wudo} bash -c "${cmd_to_run}"

echo "Now \`${target_path_for_win}\` will be loaded on start up"
echo "which is linked to ${to_link}"
