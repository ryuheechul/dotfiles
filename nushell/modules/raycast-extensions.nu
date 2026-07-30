# Reusable logic for managing a curated list of Raycast extensions.
#
# Two data sources are combined:
#   1. A curated list in raycast/config.toml (under the repo root)
#   2. Extensions installed in ~/.config/raycast/extensions/*/package.json
#
# The TUI entry point lives at bin/path/darwin/raycast-ext (bash + gum).
# Use `test-raycast-ext` from this module to verify everything works.

# ── Internal helpers ─────────────────────────────────────

def get-toml-path [] {
    # Caller must have CWD set to the repo root (both the script entry point
    # and `nu -c` invocations do this).  We use $env.PWD because inside a
    # `use`d module $env.FILE_PWD points to the runner's directory, not the
    # module's, and is therefore unreliable for locating repo-relative paths.
    $env.PWD | path join "raycast" "config.toml"
}

def scan-installed [] {
    let files = (glob ~/.config/raycast/extensions/*/package.json)
    if ($files | length) == 0 {
        return []
    }
    $files
    | each { |file| open $file | select author name title }
    | where author != null and name != null
    | each { |ext| {
        slug: $"($ext.author)/($ext.name)"
        author: $ext.author
        name: $ext.name
        title: $ext.title
    }}
}

# ── Public API ───────────────────────────────────────────

# Read the curated list from raycast/config.toml.
# Returns a list of { slug, author, name } records.
export def curated-list [] {
    let toml_path = (get-toml-path)
    if not ($toml_path | path exists) {
        return []
    }
    open $toml_path
    | get extensions
    | each { |slug|
        let parts = ($slug | split row "/")
        { slug: $slug, author: ($parts | get 0), name: ($parts | get 1) }
    }
}

# Scan installed extensions from Raycast's on-disk extension cache.
# Returns a list of { slug, author, name, title } records.
export def installed-list [] {
    scan-installed
}

# Merged view of both sources with status flags.
#
# Sort order:
#   1. in my list but not installed (need to grab on a fresh setup)
#   2. in my list and installed (all good)
#   3. installed but not in my list (candidates to add)
export def combined-list [] {
    let curated = (curated-list)
    let installed = (scan-installed)
    let curated_slugs = ($curated | get slug)
    let installed_slugs = ($installed | get slug)
    let all_slugs = ($curated_slugs | append $installed_slugs | uniq)

    $all_slugs | each { |slug|
        let parts = ($slug | split row "/")
        let in_curated = ($slug in $curated_slugs)
        let installed_match = ($installed | where slug == $slug)
        let in_installed = ($installed_match | length) > 0
        let title = (if $in_installed { $installed_match.0.title } else { "" })
        {
            slug: $slug
            author: ($parts | get 0)
            name: ($parts | get 1)
            title: $title
            curated: $in_curated
            installed: $in_installed
        }
    }
    | sort-by installed
    | sort-by curated --reverse
}

# Formatted lines for gum filter display.
# Each line: "✓★  author/name-padded-here          title"
export def display-list [] {
    combined-list
    | each { |ext|
        let installed_char = (if $ext.installed { "✓" } else { " " })
        let curated_char  = (if $ext.curated  { "★" } else { " " })
        let slug_padded   = ($ext.slug | fill -a left -w 36 -c " ")
        let title_part    = (if ($ext.title | is-not-empty) { $ext.title } else { "(not installed)" })
        $"($installed_char)($curated_char)  ($slug_padded)  ($title_part)"
    }
}

# Format a list of slugs as a multi-line TOML extensions array.
# Using `to toml` collapses single-element arrays to one line, so we
# write the array ourselves to keep entries line-separated.
export def format-extensions-toml [slugs: list<string>] {
    if ($slugs | length) == 0 {
        "extensions = []\n"
    } else {
        # Look up titles from installed extensions for inline comments
        let titles = (scan-installed | reduce -f {} {|ext, acc| $acc | upsert $ext.slug $ext.title })
        let items = ($slugs | each { |slug|
            let title = ($titles | get -o $slug | default "")
            if ($title | is-not-empty) {
                $"  # ($title)\n  \"($slug)\","
            } else {
                $"  \"($slug)\","
            }
        } | str join (char newline))
        $"extensions = [\n($items)\n]\n"
    }
}

# Add a slug to the curated list (preserves header comments).
export def add-to-curated [slug: string] {
    let toml_path = (get-toml-path)

    # File does not exist yet -- create it with header + entry
    if not ($toml_path | path exists) {
        let header = [
            "# My curated list of Raycast extensions for a fresh macOS install."
            "#"
            "# This is a bespoke approach -- Raycast has no declarative config for"
            "# managing extensions.  We keep this list and the tooling below:"
            "#"
            "#   ../nushell/modules/raycast-extensions.nu  -- data logic + tests"
            "#   ../bin/path/darwin/raycast-ext            -- TUI to browse/add/open"
            "#   ../bin/darwin/extract-raycast-extensions.nu -- PoC / one-shot extract"
            "#"
            "# Add entries as \"author/name\" from the Raycast store URL or"
            "# from the package.json of installed extensions."
            ""
        ] | str join (char newline)

        let body = (format-extensions-toml [$slug])
        $header + $body | save -f $toml_path
        print $"Created ($toml_path) with ($slug)"
        return
    }

    # Already in the list?
    let existing_slugs = (curated-list | get slug)
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

    let data      = ($raw | from toml)
    let new_slugs = ($data.extensions | append $slug)
    let body      = (format-extensions-toml $new_slugs)
    $header + $body | save -f $toml_path
    print $"Added ($slug) at ($toml_path)"
}

# Open an extension page in the Raycast app.
export def open-raycast [slug: string] {
    let parts = ($slug | split row "/")
    let url   = $"raycast://extensions/($parts.0)/($parts.1)"
    ^open $url
}

# ── Tests ────────────────────────────────────────────────
# Run with:
#   nu -c 'use nushell/modules/raycast-extensions.nu *; test-raycast-ext'

export def test-curated-list [] {
    let result = (curated-list)
    print $"curated-list: ($result | length) entries"
    if ($result | length) > 0 { $result | print }
}

export def test-installed-list [] {
    let result = (installed-list)
    print $"installed-list: ($result | length) entries"
    if ($result | length) > 0 { $result | select slug title | print }
}

export def test-combined-list [] {
    let result = (combined-list)
    print $"combined-list: ($result | length) total entries"
    print $"  curated+installed: ($result | where curated == true and installed == true | length)"
    print $"  curated only:      ($result | where curated == true and installed == false | length)"
    print $"  installed only:    ($result | where curated == false and installed == true | length)"
}

export def test-raycast-ext [] {
    print "=== Raycast Extensions Tests ==="
    print ""
    print "--- curated-list ---"
    test-curated-list
    print ""
    print "--- installed-list ---"
    test-installed-list
    print ""
    print "--- combined-list ---"
    test-combined-list
    print ""
    print "=== Done ==="
}
