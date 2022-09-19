#!/usr/bin/env bash

if [ -z "${WND_USERNAME}" ]; then
  echo 'Exiting because no $WND_USERNAME is set'
  echo "Try again like WND_USERNAME=my-wnd-user $(readlink -f $0)"
  exit 1
fi

target_path="/mnt/c/Users/${WND_USERNAME}/AppData/Roaming/Microsoft/Windows/Start Menu/Programs/Startup/reload-ahk.sh"

cat <<EOF > "${target_path}"
#!/usr/bin/env bash

cp ~/dotfiles/autohotkey/my-hotkeys.ahk ./

/mnt/c/Program\ Files/AutoHotkey/AutoHotkey.exe my-hotkeys.ahk
EOF

chmod +x "${target_path}"

echo "Now you can run \`${target_path}\` to easily reload hot keys." 
echo "Also \`~/dotfiles/autohotkey/my-hotkeys.ahk\` will be loaded on start up as well."
