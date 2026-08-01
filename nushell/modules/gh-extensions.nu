# Reusable logic for managing a curated list of gh extensions.
#
# Data source:
#   gh/extensions.toml (under the repo root)
#
# The install/upgrade task (mise/home/tasks/gh-ext-sync) loads this module
# through the mise/home/nushell symlink (-> ../../nushell), and reads the
# list with curated-list. The add TUI (mise/home/tasks/gh-ext-add) uses
# installed-list / combined-list to show what's installed on this machine so
# entries can be curated without hand-editing the TOML. The list file itself
# is read from its deployed location ~/.config/gh/extensions.toml (declared
# in mise/home/conf.d/20-dotfiles-symlinks.toml), so everything works from
# any working directory. Use `test-gh-ext` from this module to verify
# everything.

def get-toml-path [] {
    # Always the deployed location, resolved from any working directory.
    # Inside a `use`d module $env.FILE_PWD points to the runner's directory,
    # not the module's, so it can't be used to locate repo-relative paths.
    # The deployed path is a symlink to gh/extensions.toml in this repo
    # (declared in mise/home/conf.d/20-dotfiles-symlinks.toml).
    let config_home = ($env.XDG_CONFIG_HOME? | default ($env.HOME | path join ".config"))
    $config_home | path join "gh" "extensions.toml"
}

# ── Public API ───────────────────────────────────────────

# Read the curated list from gh/extensions.toml.
# Returns a list of "owner/repo" slugs.
export def curated-list [] {
    let toml_path = (get-toml-path)
    if not ($toml_path | path exists) {
        return []
    }
    open $toml_path | get extensions
}

# Scan the gh extensions installed on this machine.
# Uses `gh ext list`, which already knows each extension's owner/repo (no
# digging through the install dirs). Returns a list of installed
# "owner/repo" slugs. Rows without a repo - local dev checkouts, e.g.
# symlinked ones - are skipped: they aren't portable and can't be curated.
export def installed-list [] {
    let rows = (try { gh ext list | lines | where { |l| ($l | str trim | is-not-empty) } } catch { [] })
    if ($rows | is-empty) {
        return []
    }
    $rows
    | each { |l|
        ($l | split row (char tab) | get 1? | default "" | str trim)
    }
    | where { |slug| $slug =~ '^[^/\s]+/[^/\s]+$' }
}

# Merged view of the curated list and what's installed on this machine.
# Returns a list of { slug, name, installed, curated } records,
# sorted with installed-but-not-curated entries first (the actionable ones
# for the add TUI), then curated+installed, then curated-but-not-installed.
export def combined-list [] {
    let curated = (curated-list)
    let installed = (installed-list)
    let all_slugs = ($curated | append $installed | uniq)

    $all_slugs
    | each { |slug|
        let is_installed = ($slug in $installed)
        let is_curated   = ($slug in $curated)
        {
            slug: $slug
            name: ($slug | split row "/" | last)
            installed: $is_installed
            curated: $is_curated
            # single sort key (nu sort-by is unstable across chained sorts):
            # installed-but-not-curated first (actionable in the add TUI),
            # then curated+installed, then curated-but-not-installed
            rank: (if (not $is_curated) and $is_installed {
                0
            } else if $is_installed and $is_curated {
                1
            } else {
                2
            })
        }
    }
    | sort-by rank
    | reject rank
}

# Format a list of { slug, comment } entries as a multi-line TOML
# extensions array, with each entry's comment (if any) on the line above it.
# Using `to toml` collapses single-element arrays to one line, so we
# write the array ourselves to keep entries line-separated.
# The header comment block above `extensions = [` is preserved by the caller.
export def format-extensions-toml [entries] {
    if ($entries | length) == 0 {
        "extensions = []\n"
    } else {
        let items = ($entries | each { |e|
            let comment_line = (if ($e.comment | default "" | str trim | is-not-empty) {
                $"  # ($e.comment | str trim)"
            } else { "" })
            [$comment_line, $"  \"($e.slug)\","]
            | where { |line| $line != "" }
            | str join (char newline)
        } | str join (char newline))
        $"extensions = [\n($items)\n]\n"
    }
}

# Parse gh/extensions.toml into { slug, comment } records, keeping the
# comment line(s) that sit directly above each entry so rewrites can
# preserve them (e.g. "# PR/issue dashboard TUI" above "dlvhdr/gh-dash").
export def get-entries-with-comments [] {
    let toml_path = (get-toml-path)
    if not ($toml_path | path exists) {
        return []
    }
    let body = (open --raw $toml_path
        | lines
        | skip while { |line| ($line | str trim) != "extensions = [" }
        | skip 1
        | take while { |line| ($line | str trim) != "]" })
    mut result = []
    mut pending = ""
    for line in $body {
        let trimmed = ($line | str trim)
        if ($trimmed | str starts-with "#") {
            # accumulate consecutive comment lines above an entry
            $pending = (if $pending == "" {
                ($trimmed | str replace -r '^#\s*' '')
            } else {
                $pending + " " + ($trimmed | str replace -r '^#\s*' '')
            })
        } else if ($trimmed | str starts-with '"') and ($trimmed | str ends-with '",') {
            let slug = ($trimmed | str trim -c ',' | str trim -c '"')
            $result = ($result | append { slug: $slug, comment: $pending })
            $pending = ""
        }
    }
    $result
}

# One-line description for a repo slug, used as the comment above a newly
# curated entry. Empty when unreachable (offline, not found, no gh auth).
export def get-repo-description [slug: string] {
    (try {
        gh repo view $slug --json description --jq .description
    } catch { "" })
    | str trim
}

# Add a slug to the curated list (preserves header and per-entry comments).
export def add-to-curated [slug: string] {
    let toml_path = (get-toml-path)

    # File does not exist yet -- create it with header + entry
    if not ($toml_path | path exists) {
        let header = [
            "# Curated list of gh extensions to install/upgrade on any machine."
            "#"
            "# This is a bespoke approach -- gh has no declarative extension"
            "# config, so we keep a curated list here and manage it with the"
            "# tooling below:"
            "#"
            "#   ../nushell/modules/gh-extensions.nu -- data logic + tests"
            "#   ../mise/home/tasks/gh-ext            -- installs/upgrades"
            "#"
            "# Add entries as \"owner/repo\" from the GitHub repo URL."
            "# Installs are declarative: an entry gets installed only if it's"
            "# missing, so re-running never reinstalls what's already there."
            "# Extensions installed but not listed are left alone (upgraded"
            "# but not removed)."
            ""
        ] | str join (char newline)

        let body = (format-extensions-toml [{ slug: $slug, comment: (get-repo-description $slug) }])
        $header + $body | save -f $toml_path
        print $"Created ($toml_path) with ($slug)"
        return
    }

    # Already in the list?
    let existing_slugs = (curated-list)
    if $slug in $existing_slugs {
        print $"Already in list: ($slug)"
        return
    }

    # Preserve header comments, write array body in multi-line format
    let raw         = (open --raw $toml_path)
    let all_lines   = ($raw | lines)
    let comment_lines = ($all_lines | take while { |line| ($line | str starts-with "#") or $line == "" })
    let header = if ($comment_lines | length) > 0 {
        ($comment_lines | str join (char newline)) + (char newline)
    } else { "" }

    # Keep existing per-entry comments and add the new entry with its GitHub
    # description as the comment above it
    let new_entry = { slug: $slug, comment: (get-repo-description $slug) }
    let entries   = (get-entries-with-comments | append $new_entry)
    let body      = (format-extensions-toml $entries)
    $header + $body | save -f $toml_path
    print $"Added ($slug) at ($toml_path)"
}

# ── Tests ────────────────────────────────────────────────
# Run with:
#   nu -c 'use nushell/modules/gh-extensions.nu *; test-gh-ext'

def test-installed-list [] {
    let result = (installed-list)
    print $"installed-list: ($result | length) entries"
    if ($result | length) > 0 { $result | print }
}

def test-combined-list [] {
    let result = (combined-list)
    print $"combined-list: ($result | length) total entries"
    print $"  curated+installed: ($result | where curated == true and installed == true | length)"
    print $"  curated only:      ($result | where curated == true and installed == false | length)"
    print $"  installed only:    ($result | where curated == false and installed == true | length)"
}

export def test-gh-ext [] {
    print "=== gh Extensions Tests ==="
    print ""
    print "--- curated-list ---"
    let result = (curated-list)
    print $"curated-list: ($result | length) entries"
    if ($result | length) > 0 { $result | print }

    print ""
    print "--- installed-list ---"
    test-installed-list

    print ""
    print "--- combined-list ---"
    test-combined-list

    print ""
    print "--- add-to-curated smoke test ---"
    let slug = "test/verify-path"
    let toml_path = (get-toml-path)
    # byte-exact snapshot; restored below so the test never mutates the list
    let snapshot = (try { open --raw $toml_path } catch { "" })
    add-to-curated $slug
    let found = (curated-list | where $in == $slug | length)
    if $found > 0 {
        print $"✓ added and read back ($slug)"
        if $snapshot == "" {
            # file did not exist before the test - restore that absence
            rm -f $toml_path
        } else {
            $snapshot | save -f $toml_path
        }
        print "✓ restored the file byte-for-byte"
    } else {
        print "✗ failed to add test entry"
    }
    print ""
    print "=== Done ==="
}
