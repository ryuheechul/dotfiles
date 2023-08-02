{ ... }:


# https://nixos.wiki/wiki/Flatpak
# https://flatpak.org/setup/NixOS
# `flatpak remote-add --if-not-exists flathub https://flathub.org/repo/flathub.flatpakrepo`
# `export XDG_DATA_DIRS=$XDG_DATA_DIRS:/usr/share:/var/lib/flatpak/exports/share:$HOME/.local/share/flatpak/exports/share`
{
  services.flatpak.enable = true;
}
