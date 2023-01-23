#!/usr/bin/env bash

set -e

tempfile="$(mktemp)"
cp /etc/wsl.conf "${tempfile}"
echo "previous file's at ${tempfile} and its content was:"
cat "${tempfile}"

# should run this script with sudo because of writing to /etc/wsl.conf
cat <<EOF > /etc/wsl.conf
[boot]
systemd=true
[network]
generateResolvConf=false
EOF

# relavent links to explain options
# - https://devblogs.microsoft.com/commandline/systemd-support-is-now-available-in-wsl/
# - https://askubuntu.com/a/1398053/1666783

echo
echo "and printing the new one"
echo 'cat /etc/wsl.conf'
cat /etc/wsl.conf

echo
echo 'Rebooting would require to take effects on new changes.'
