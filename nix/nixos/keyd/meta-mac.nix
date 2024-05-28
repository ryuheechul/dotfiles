# see ./settings-min.nix and ./settings-full.nix for usages
# ../../home/dconf.nix is also being used to customize shortcuts in settings

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
  w = "C-w";
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
  equal = "C-+";
  # Zoom out
  "-" = "C--";
  # Move cursor to beginning of line
  left = "home";
  # Move cursor to end of Line
  right = "end";
  # Switch between windows
  "`" = "A-f6";
}
