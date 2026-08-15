# age-ssh-nt: an age-agent-like experience for passage

`age` has no agent: identities are plaintext files, so decrypting from several
devices means copying key material around. `age-ssh-nt` bridges the missing
age-agent experience instead - a single encrypted identities file whose
recipients are derived from SSH keys (via the `age-plugin-sshagent` plugin),
so any enrolled key in an SSH agent can decrypt it on demand, and only during
a `passage` invocation. No plaintext key material is kept persistently; the
Passage bridge briefly uses the private temporary file described below.

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
the state files it describes live in `$AGE_SSH_NT_HOME` (a plain directory,
created by `bootstrap`):

| file | secret? | purpose |
|---|---|---|
| `identities.age` | yes (0600) | the passage identities, age-encrypted to all enrolled recipients |
| `identities.age.recipients-sha256` | no | canonical hash of the exact `authorized_recipients` key set used for the current `identities.age` |
| `identities.age.rotation.bak` | yes (0600) | the previous encrypted passage identities, retained by `rotate` until the new identity is verified against every store entry |
| `authorized_recipients` | no | one `age1...` recipient per enrolled key, each under a comment naming the key - an open-ended, purely additive list |
| `*.identity` | no | per-key plugin identity (key fingerprint + salt); only useful while the matching key is in the SSH agent |
| `store_recipient` | no | the identities' own public key, recorded by `bootstrap`/`reencrypt` so `status` can check the store side without needing the agent |
| `README.md` | no | tiny pointer dropped by `bootstrap`, links to this documentation (`age-ssh-nt doc`) |

`~/.passage/` holds only genuine passage files (`store/`, `.age-recipients`).
`$PASSAGE_DIR/.age-recipients` is passage's own mechanism (the recipients
new store entries are encrypted to). The bootstrap step also appends the
encrypted identities' own public key there (under a comment explaining what it
is and pointing at `$AGE_SSH_NT_HOME`), so `passage insert`/`edit` keep
encrypting to the same identity that `age-ssh-nt passage` hands out.
Before age-ssh-nt rewrites an existing valid recipient list, it atomically
copies it to `$PASSAGE_DIR/.age-recipients.age-ssh-nt.bak`. This non-secret,
last-known-good copy is ignored by Passage. An invalid current list never
overwrites a valid backup, and unsafe backup objects such as symlinks or
directories block mutations until inspected.
The backup starts with a generated-file comment explaining that it is
overwritten by the next recipient rewrite and is safe to delete after
`age-ssh-nt status` confirms the current `.age-recipients` is valid. Restoring
the backup and later rewriting recipients does not duplicate this header.
Managed comment/key blocks are separated from existing content by exactly one
blank line.

## The one script (in `bin/path/ssh/`)

`age-ssh-nt` covers the whole lifecycle:

| subcommand | what it does |
|---|---|
| `age-ssh-nt bootstrap` | one-shot bootstrap on a new box - generates a fresh age identity in memory (`age-keygen`, never saved to disk), prints a manifest of exactly what will happen (including which SSH key will be used), asks for confirmation, then registers an SSH agent key (picked from a menu if the agent holds several), encrypts the identities to it, appends the identities' own public key to `$PASSAGE_DIR/.age-recipients` and verifies decryption |
| `age-ssh-nt enroll` | enroll one more device key: picks a key from the SSH agent (menu if several), derives the name from the key's comment and asks for confirmation - purely additive, never decrypts |
| `age-ssh-nt reencrypt` | re-encrypt `identities.age` in place to all enrolled recipients (needs an already-enrolled key in the SSH agent; run after `enroll`) |
| `age-ssh-nt rotate` | replace the passage identity, retain the old encrypted identity and store recipient as a rotation backup, and re-encrypt the store to old + new |
| `age-ssh-nt rotate finalize` | verify every store file with the new identity, remove the old recipient, re-encrypt the store, and remove the encrypted rotation backup |
| `age-ssh-nt status` | report what is done and what still needs doing on both sides - strictly read-only, never changes anything (see [Checking status](#checking-status)) |
| `age-ssh-nt passage <passage args>` | run passage with the identities decrypted on the fly - `passage reencrypt` is passed through and re-encrypts the store files to `.age-recipients` |
| `age-ssh-nt doc` | open this documentation with `glow` (falls back to `less`/`cat` if glow is not installed) |
| `age-ssh-nt help` | usage |

State-changing commands are serialized by
`$AGE_SSH_NT_HOME/.mutation.lock`; concurrent mutations fail with the owning
PID instead of sharing temporary or backup files. The lock is removed on
normal exit, errors, INT and TERM. After an uncatchable SIGKILL, confirm that
the recorded process is gone before removing the stale lock directory.
Lifecycle failures also point to `age-ssh-nt status`, which reports the current
state and recovery steps. Argument errors and deliberate aborts omit that hint.

The script honors `PASSAGE_DIR` (default `~/.passage/store`) and
`AGE_SSH_NT_HOME` (default `$XDG_DATA_HOME/dfs-rhc/age-ssh-nt`, falling back to
`~/.local/share/dfs-rhc/age-ssh-nt`; it holds `identities.age`,
`authorized_recipients` and the `*.identity` files). The store recipient list
is `$PASSAGE_DIR/.age-recipients`.

When `AGE_SSH_NT_HOME` is unset, existing state is selected in this order:
the new `dfs-rhc` XDG location, the previous `$XDG_DATA_HOME/age-ssh-nt`
location, then legacy `~/.age/age-ssh-nt`. New state uses the `dfs-rhc`
location.

For encryption, `authorized_recipients` is canonicalized in a temporary file:
comments and blank lines are removed, then key lines are sorted and
deduplicated. The human-edited source file is never rewritten implicitly;
`status` reports duplicate lines as pending cleanup.
Store recipients remain under Passage's control and are not deduplicated before
encryption. `status` therefore reports duplicate `.age-recipients` key lines
before recommending re-encryption; remove the duplicates first, then run the
single re-encryption that removes their duplicate ciphertext stanzas.

## Decryption

`age-ssh-nt passage` tries each `*.identity` one at a time until one decrypts
(age aborts on the first plugin identity whose key is not in the agent, so
passing all identities in one `age -d -i` call does not work). The SSH agent
must hold one of the enrolled keys - locally on the box, or forwarded from the
device via `ssh -A`.

The decrypted identities briefly occupy a private 0600 named temp file. On
Linux it is opened as fd 3 and immediately unlinked after decryption, then
`passage` reads `PASSAGE_IDENTITIES_FILE=/dev/fd/3` (each `age -d` re-opens it
at offset 0, so `passage edit`'s two reads both work; the kernel reclaims the
anonymous inode when the script exits, even on SIGKILL). The named-file window
is normally in `XDG_RUNTIME_DIR` (tmpfs = RAM, wiped on reboot), but `$TMPDIR`
or `/tmp` fallbacks may be disk-backed.

On macOS/BSD, opening `/dev/fd/N` is equivalent to `dup(N)` (fdescfs) and
shares the file offset, so a second read would see EOF. There the identities
remain in a named 0600 file under `$TMPDIR` and the cleanup trap removes it on
normal exit, errors, INT and TERM. An uncatchable SIGKILL can leave that
private file until the OS cleans up the temporary directory.

## Enrolling a device (purely additive)

Enrollment needs the device's own agent (one signature to prove possession),
never decrypts anything, and never affects previously enrolled devices:

> **Warning:** only forward an agent to a host you trust. During enrollment,
> the remote host can request the deterministic derivation signature and
> retain the resulting age private key permanently. An agent confirmation
> prompt lets you approve the signature request, but it cannot prevent the
> requesting host from keeping the signature or derived key afterward.

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

The pinned plugin supports ordinary `ssh-ed25519` keys only. RSA, ECDSA and
FIDO/security-key variants are omitted from the selection menu; if the agent
contains no compatible key, enrollment exits with an explicit error.

If enrollment is interrupted after the non-secret `*.identity` file is
created but before its recipient is appended, running `age-ssh-nt enroll`
again with the same selected key reports the partial state and repairs the
missing recipient without generating a different identity.

New plugin identities are generated and checked in a unique sibling temporary
directory, then atomically installed. If another enrollment claims the same
comment-derived filename while confirmation is pending, the later enrollment
stops without touching the existing identity and asks to be rerun.

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

It prints a manifest (back up and rewrite `identities.age`, update the store
recipients, re-encrypt `$PASSAGE_DIR`) and asks for confirmation. When store
files exist it first offers an optional read-only check that decrypts each file
to `/dev/null` with the current identity. Failures are listed with a hint to
inspect them through `age-ssh-nt passage`, but do not block rotation; the final
confirmation decides whether to proceed.

Before Passage runs, rotation restores the exact old store recipient if it is
missing, adds the new one, and verifies that both lines are present. It then
runs one re-encryption pass to old + new.
Because `passage reencrypt` is not assumed to be all-or-nothing, the old
encrypted identities are renamed to `identities.age.rotation.bak` and the old
store recipient remains in `.age-recipients`, marked with a cleanup comment.
No plaintext backup is written.

Recipient-list changes use an atomic sibling rewrite and first refresh the
durable `.age-recipients.age-ssh-nt.bak`. Rotation also keeps a separate temporary sibling
snapshot of the original `.age-recipients` and restores it when verification,
Passage, or identity installation fails. A SIGKILL can leave that snapshot and
the pending marker behind; `status` reports the interrupted pre-install state
and the original recipient snapshot path.

`status` reports the rotation as pending while the rotation backup exists, and a
second rotation is refused. Finish it from a session whose agent holds an
enrolled key:

```sh
age-ssh-nt rotate finalize
```

Finalization decrypts both the current and backup identities to derive their
exact recipients, so it does not trust manually edited marker comments. It
refuses to continue unless every store file decrypts with the current identity,
then prints a manifest and asks for explicit confirmation. It atomically
removes the old recipient and managed rotation note, restores the standard
age-ssh-nt comment immediately above the current recipient, runs Passage
re-encryption, verifies every store file again, and only then removes
`identities.age.rotation.bak`. Failures restore the recipient list and retain
the encrypted backup.

If an entry works only with the old identity, restore it before troubleshooting:

```sh
mv "$AGE_SSH_NT_HOME/identities.age" "$AGE_SSH_NT_HOME/identities.age.failed-rotation"
mv "$AGE_SSH_NT_HOME/identities.age.rotation.bak" "$AGE_SSH_NT_HOME/identities.age"
age-ssh-nt reencrypt   # also restores the store_recipient status sidecar
```

Rotation changes only the inner passage identity, not the SSH-derived wrapper.
The plugin `*.identity` salt and outer recipient remain unchanged, so anyone
who can make the agent sign - or who retained the deterministic signature for
that salt - can still derive the wrapper key and decrypt newly rotated
`identities.age`. Invalidating such a signature requires replacing that outer
plugin identity and recipient, which is intentionally outside this command's
scope.

## Fresh setup

```sh
age-ssh-nt bootstrap   # first command on a new box
```

It prints a manifest of exactly what it will do (create
`$AGE_SSH_NT_HOME`, the key registration, `authorized_recipients` and
`identities.age`;
append the identities' public key to `$PASSAGE_DIR/.age-recipients`) and
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

`identities.age` is written to a sibling temporary file, verified, and then
installed atomically. If bootstrap is interrupted after that rename but before
its sidecar or store recipient is written, running `age-ssh-nt bootstrap`
again cryptographically decrypts the existing file before repairing missing
metadata from its real contents. An incomplete, fabricated or invalid file is
reported explicitly rather than being treated as a completed bootstrap.

The recipient-set hash is likewise prepared in a unique sibling file and its
destination must be a regular non-symlink file. Identity replacement is rolled
back (or an incomplete first bootstrap is removed) if the hash cannot be
installed.

If `authorized_recipients` already exists (seeded from another box), bootstrap
does not register a key: it still generates fresh identities in memory and
encrypts them to the existing recipient list. The identities are always newly
generated - plaintext is never kept, so nothing can be reused or recovered.

Back up `$AGE_SSH_NT_HOME/identities.age` together with `$PASSAGE_DIR/` -
the identities file is required to decrypt the store, and it is the only
secret file in this directory.

## Checking status

The whole setup spans two sides, each with its own re-encryption step, so
`age-ssh-nt status` reports what is done and what is still pending on both:

```sh
age-ssh-nt status
```

It is strictly read-only - it never writes or changes anything - and combines
several checks:

- **Exact identities recipient set**: after writing `identities.age`, the
  script records a hash of the sorted, deduplicated key lines from
  `authorized_recipients`. Status compares both that hash and the age stanza
  count, so replacing one recipient with another is detected even when the
  count stays the same. A missing hash is reported as unverified and healed by
  `age-ssh-nt reencrypt`.
- **Cryptographic identity validation**: when an enrolled key is present in the
  current agent, status must successfully decrypt `identities.age` before
  calling its ciphertext valid. If the matching fingerprint is present but
  decryption fails, status reports an invalid/plugin-failure state and a
  pending recovery action. Without a matching agent it reports that validity
  cannot be verified rather than inferring validity from header-shaped text.
- **Store recipient-count staleness**: every store file's age header records
  one recipient stanza (`-> X25519 ...`) per recipient it was encrypted to.
  Each `*.age` file is compared against `$PASSAGE_DIR/.age-recipients`; zero
  readable stanzas are reported as an invalid file rather than up to date.
- **Recipient backup safety**: status validates both `.age-recipients` and its
  durable `.age-recipients.age-ssh-nt.bak`, reports unsafe backup objects that block
  mutations, and points to the backup when the current list is missing or
  invalid.
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
