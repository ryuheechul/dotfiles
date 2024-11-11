# see `./settings-min.nix` and `./settings-full-windows.nix` for usages
# `../../home/dconf.nix` is also being used to customize shortcuts in settings
# List available keys with `nix-shell -p keyd --command 'keyd list-keys'`.

{
  # Copy
  c = "C-insert";
  # Paste
  v = "S-insert";
  # Cut
  x = "S-delete";
  # Select all
  a = "C-a";
  # Undo (and redo with shift)
  z = "C-z";
  # Close window
  w = "C-w"; # note that this is not very universal in gnome use `C-q` below to close (single window) app
  # Close app - but directly setting this actually interferes with `C-A-Q` which is I'm mimicking macOS for locking screen;
  #   which is ok as long as I set `A-q` for closing windows with `../../home/dconf.nix`
  # q = "C-q";
  # Refresh
  r = "C-r";
  # Settings
  "," = "C-,";
  # Save
  s = "C-s";
  # New tab
  t = "C-t";
  # New window
  n = "C-n";
  # Cmd+K: usually quick access or search on my websites
  k = "C-k";
  # Find
  f = "C-f";
  # Next (and previous with shift) item (usually with) find
  g = "C-g";
  # Focus address bar
  l = "C-l";
  # comment uncomment
  "/" = "C-/";
  # Zoom in: `=` would not work, so `equal`
  equal = "C-="; # `equal = "C-+"` was a mistake which only worked with some applications not all
  # Zoom out
  "-" = "C--";
  # go backward
  "[" = "C-[";
  # go forward
  "]" = "C-]";
  # Move cursor to beginning of line
  left = "home";
  # Move cursor to end of Line
  right = "end";
  # Switch between windows
  "`" = "A-f6";
}
