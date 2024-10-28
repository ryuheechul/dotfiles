{ ... }:

# https://nixos.wiki/wiki/Flatpak
# https://nixos.org/manual/nixos/stable/index.html#module-services-flatpak
# https://flatpak.org/setup/NixOS
# https://wiki.archlinux.org/title/Flatpak
# `flatpak remote-add --if-not-exists flathub https://flathub.org/repo/flathub.flatpakrepo`
# `export XDG_DATA_DIRS=$XDG_DATA_DIRS:/usr/share:/var/lib/flatpak/exports/share:$HOME/.local/share/flatpak/exports/share`
{
  imports = [
    ../../pkgs/custom/nix-flatpak.nix # to import https://github.com/gmodena/nix-flatpak module
  ];

  services.flatpak.enable = true;

  # declarative flatpak package management powered via `nix-flatpak`
  services.flatpak.packages = [
    "com.github.tchx84.Flatseal" # Flatseal
    "com.google.Chrome" # Google Chrome
    "io.github.arunsivaramanneo.GPUViewer" # GPU-Viewer
    "io.github.zen_browser.zen" # Zen
    "io.github.plrigaux.sysd-manager" # a GUI to manage systemd units
    # "net.lutris.Lutris" # Lutris
    # "com.slack.Slack" # Slack
  ];
}
