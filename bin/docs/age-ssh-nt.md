# age-ssh-nt: an age-agent-like experience for passage

`age` has no agent: identities are plaintext files, so decrypting from several
devices means copying key material around. `age-ssh-nt` bridges the missing
age-agent experience instead - a single encrypted identities file whose
recipients are derived from SSH keys (via the `age-plugin-sshagent` plugin),
so any enrolled key in an SSH agent can decrypt it on demand, and only during
a `passage` invocation. No plaintext key material ever rests on disk.

From `passage`'s perspective this adds no complexity: the identities are just
another recipient it decrypts with. Enrolling another SSH key only
re-encrypts `identities.age` to the bigger recipient list (`age-ssh-nt
reencrypt`) - `passage` never needs to know which SSH key happened to sign.
It is fully transparent as long as passage is invoked through the wrapper
(`age-ssh-nt passage`); for convenience, `zsh/integration/interactive-ssh`
aliases `passage` to `age-ssh-nt passage` on SSH connections, so the box
always sees a plain `passage`.

`age-plugin-sshagent` could also be used on its own: point passage at one
plaintext identity file per SSH key. That is more direct, but each key then
becomes its own recipient, so adding a key means re-encrypting the whole store
to the new recipient. `age-ssh-nt` instead keeps passage on a single, stable
recipient no matter how many SSH keys come and go - the trade-off this design
prefers.

Note: `age-plugin-sshagent` defines no file layout of its own (identity files
are written wherever `-o` says, "safe to store anywhere"). This directory
layout is our convention.

## Files

This file is the documentation and lives in the repo at `bin/docs/age-ssh-nt.md`;
the state files it describes live in `~/.age/age-ssh-nt/` (a plain directory,
created by `bootstrap`):

| file | secret? | purpose |
|---|---|---|
| `identities.age` | yes (0600) | the passage identities, age-encrypted to all enrolled recipients |
| `authorized_recipients` | no | one `age1...` recipient per enrolled key, each under a comment naming the key - an open-ended, purely additive list |
| `*.identity` | no | per-key plugin identity (key fingerprint + salt); only useful while the matching key is in the SSH agent |
| `store_recipient` | no | the identities' own public key, recorded by `bootstrap`/`reencrypt` so `status` can check the store side without needing the agent |
| `README.md` | no | tiny pointer dropped by `bootstrap`, links to this documentation (`age-ssh-nt doc`) |

`~/.passage/` holds only genuine passage files (`store/`, `.age-recipients`).
`~/.passage/store/.age-recipients` is passage's own mechanism (the recipients
new store entries are encrypted to). The bootstrap step also appends the
encrypted identities' own public key there (under a comment explaining what it
is and pointing at `~/.age/age-ssh-nt/`), so `passage insert`/`edit` keep
encrypting to the same identity that `age-ssh-nt passage` hands out.

## The one script (in `bin/path/ssh/`)

`age-ssh-nt` covers the whole lifecycle:

| subcommand | what it does |
|---|---|
| `age-ssh-nt bootstrap` | one-shot bootstrap on a new box - generates a fresh age identity in memory (`age-keygen`, never saved to disk), prints a manifest of exactly what will happen (including which SSH key will be used), asks for confirmation, then registers an SSH agent key (picked from a menu if the agent holds several), encrypts the identities to it, appends the identities' own public key to `~/.passage/store/.age-recipients` and verifies decryption |
| `age-ssh-nt enroll` | enroll one more device key: picks a key from the SSH agent (menu if several), derives the name from the key's comment and asks for confirmation - purely additive, never decrypts |
| `age-ssh-nt reencrypt` | re-encrypt `identities.age` in place to all enrolled recipients (needs an already-enrolled key in the SSH agent; run after `enroll`) |
| `age-ssh-nt status` | report what is done and what still needs doing on both sides - strictly read-only, never changes anything (see [Checking status](#checking-status)) |
| `age-ssh-nt passage <passage args>` | run passage with the identities decrypted on the fly - `passage reencrypt` is passed through and re-encrypts the store files to `.age-recipients` |
| `age-ssh-nt doc` | open this documentation with `glow` (falls back to `less`/`cat` if glow is not installed) |
| `age-ssh-nt help` | usage |

The script honors `AGE_HOME` (default `~/.age`), `PASSAGE_HOME` (default
`~/.passage`) and `AGE_SSH_NT_HOME` (default `$AGE_HOME/age-ssh-nt`, holding
`identities.age`, `authorized_recipients` and the `*.identity` files). The
store recipient list is `$PASSAGE_HOME/store/.age-recipients`.

## Decryption

`age-ssh-nt passage` tries each `*.identity` one at a time until one decrypts
(age aborts on the first plugin identity whose key is not in the agent, so
passing all identities in one `age -d -i` call does not work). The SSH agent
must hold one of the enrolled keys - locally on the box, or forwarded from the
device via `ssh -A`.

The decrypted identities never rest on disk: on Linux the temp file is
unlinked right after being opened as fd 3, and `passage` reads
`PASSAGE_IDENTITIES_FILE=/dev/fd/3` (each `age -d` re-opens it at offset 0, so
`passage edit`'s two reads both work; the kernel reclaims the anonymous inode
when the script exits - even on SIGKILL). On macOS/BSD, opening `/dev/fd/N` is
a `dup()` that shares the file offset (fdescfs), so a second read would see
EOF; there the identities stay in a named 0600 file under `$TMPDIR`, removed
by an EXIT trap (fires on exit, errors, INT and TERM; a SIGKILL could only
leave a private 0600 file until the OS cleans it up). The temp file lives in
`XDG_RUNTIME_DIR` on Linux (tmpfs = RAM, wiped on reboot), `$TMPDIR` on macOS,
`/tmp` as last resort.

## Enrolling a device (purely additive)

Enrollment needs the device's own agent (one signature to prove possession),
never decrypts anything, and never affects previously enrolled devices:

1. From the new device: `ssh -A <box>`, then
   `age-ssh-nt enroll` (approve the signature prompt). It picks a key from the
   device's SSH agent (a numbered menu if the agent holds several), derives
   the identity name from the key's comment, and asks for confirmation. Then
   it appends the new `age1...` recipient. Enrollment is purely additive and
   never touches the identities. The box must be bootstrapped first - `enroll`
   refuses otherwise, since `bootstrap` already includes the first enrollment.
2. From the box (or any machine whose agent holds an enrolled key):
   `age-ssh-nt reencrypt` - decrypts with an enrolled key and
   re-encrypts to all recipients.

Keys that live only on their own device (e.g. iOS Secure Enclave keys) are
enrolled from the device via step 1 - they never need to exist in the box's
agent.

One caveat: enrollment derives the age identity from a signature the agent
produces, so it only works with agents that sign deterministically. If the
device's agent does not, enrollment fails with an explicit error ("agent
produced non-deterministic signatures... this key cannot be used") and that
key simply cannot become an age identity. This depends on the agent, not on
the key - the same key can fail through one agent and succeed through another.

## Rotating the identities

The store side has one global secret: the passage identities sealed in
`identities.age`. Rotating them replaces that identity with a fresh one and
re-encrypts the store to it - the SSH side (`authorized_recipients`, the
`*.identity` files) is untouched, so every device keeps working:

```sh
age-ssh-nt rotate   # from any machine whose agent holds an enrolled key
```

It prints a manifest (rewrite `identities.age`, update the store recipients,
re-encrypt `~/.passage/store/`), asks for confirmation, then runs in two
re-encrypt passes: first to [old + new] so the store stays decryptable by a
live identity at every step, then - after the fresh identity is verified in
place - to the new identity only, trimming the old key from
`~/.passage/store/.age-recipients`. A crash at any point leaves the store
readable by one of the identities, and re-running converges.

Rotation is for rotating the store secret, not the SSH keys. Because the
wrapper key is derived from an agent signature, anyone who can make the agent
sign can re-derive it - rotation does not change that. What it does fix is the
one-time-leak class: a captured signature is bound to its salt (each fresh
identity uses a new salt), so anything observed before the rotation is
worthless after it.

## Fresh setup

```sh
age-ssh-nt bootstrap   # first command on a new box
```

It prints a manifest of exactly what it will do (create
`~/.age/age-ssh-nt/`, the key registration, `authorized_recipients` and
`identities.age`;
append the identities' public key to `~/.passage/store/.age-recipients`) and
shows which SSH key it will use, then asks for confirmation before doing
anything. After a `y`, it registers an SSH agent key (a numbered menu appears
if the agent holds several), generates a fresh age identity in memory
(`age-keygen`, never saved to disk - so there is never a plaintext to delete)
and encrypts it. Decryption is verified. More keys are added later with
`age-ssh-nt enroll` (picks the key from the SSH agent, confirms the derived
name) followed by `age-ssh-nt reencrypt`. Afterwards, confirm the
store still decrypts:

```sh
age-ssh-nt passage show <entry>
```

If `authorized_recipients` already exists (seeded from another box), bootstrap
does not register a key: it still generates fresh identities in memory and
encrypts them to the existing recipient list. The identities are always newly
generated - plaintext is never kept, so nothing can be reused or recovered.

Back up `~/.age/age-ssh-nt/identities.age` together with `~/.passage/store/` -
the identities file is required to decrypt the store, and it is the only
secret file in this directory.

## Checking status

The whole setup spans two sides, each with its own re-encryption step, so
`age-ssh-nt status` reports what is done and what is still pending on both:

```sh
age-ssh-nt status
```

It is strictly read-only - it never writes or changes anything - and combines
three kinds of checks:

- **Recipient-count staleness**: every age file's header records one recipient
  stanza (`-> X25519 ...`) per recipient it was encrypted to, and additive
  changes are visible as a shortfall in that count. `identities.age` is
  compared against `authorized_recipients` (after `enroll` appends a
  recipient, status reports `identities.age stale: encrypted to X of N
  recipient(s)` with the pending step `age-ssh-nt reencrypt`), and each
  `*.age` store file against `~/.passage/store/.age-recipients` (after
  `bootstrap` appends the identities' own key, pre-existing files are stale
  until `age-ssh-nt passage reencrypt`).
- **Real key-line check**: the `recipients` row matches the identities' actual
  `age1...` public key (derived from the decrypted identities, or read from
  the `store_recipient` sidecar when no agent is present) against the store
  recipient list line by line - it reports whether *new* store entries will
  encrypt to the age-ssh-nt identity. Comment markers alone are never
  trusted.
- **Actual decryption**: when an enrolled key is in the current SSH agent,
  status decrypts the identities and uses them to decrypt a store file,
  answering the question that matters - `store readable: yes` - even when the
  recipient counts happen to coincide. Without an enrolled key it says
  `cannot verify` instead of guessing. It also reports why decrypting the
  identities right now would fail, if it would (no agent, a dead socket, or
  no matching key).

When everything is in sync the output ends with `all set - nothing pending`;
otherwise a `pending:` list names exactly the missing steps. This is the
go-to command whenever a device cannot decrypt something that another one
can.
