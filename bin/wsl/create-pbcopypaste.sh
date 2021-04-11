#!/usr/bin/env bash

# thanks to https://www.techtronic.us/pbcopy-pbpaste-for-wsl/

mkdir -p ~/.local/bin/

cat <<EOF > ~/.local/bin/pbcopy
#!/usr/bin/env bash
# pbcopy for wsl
tee <&0 | clip.exe
exit 0
EOF

cat <<EOF > ~/.local/bin/pbpaste
#!/usr/bin/env bash
# pbpaste for WSL
powershell.exe Get-Clipboard | sed 's/\r$//' | sed -z '$ s/\n$//'
exit 0
EOF

chmod +x ~/.local/bin/pbcopy
chmod +x ~/.local/bin/pbpaste
