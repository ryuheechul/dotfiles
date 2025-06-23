const script_dir = path self .

def open-rel [rel_path: string] {
  $script_dir | path join $rel_path | open $in
}

export def get-files [] {
  let adp = {
    on: (open-rel ./adp/on.txt),
    off: (open-rel ./adp/off.txt),
  }

  let bat = {
    full: (open-rel ./bat/full.txt),
    unknown: (open-rel ./bat/unknown.txt),
    charging: (open-rel ./bat/charging.txt),
    discharging: (open-rel ./bat/discharging.txt),
    not_charging: (open-rel ./bat/not-charging.txt),
  }

  {
    adp: $adp,
    bat: $bat,
  }
}
