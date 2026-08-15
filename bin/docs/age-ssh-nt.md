# age-ssh-nt: an age-agent-like experience for passage

`age` has no agent: identities are plaintext files, so decrypting from several
devices normally means copying key material around. `age-ssh-nt` instead keeps
one encrypted Passage identities file whose recipients are derived from SSH
keys through `age-plugin-sshagent`. Any enrolled key in an SSH agent can decrypt
it on demand during `age-ssh-nt passage`; no plaintext identity is kept
persistently.

Passage sees one stable inner recipient regardless of how many SSH keys are
enrolled. Adding a device only re-encrypts `identities.age` to a larger outer
recipient set, so Passage never needs to know which SSH key signed. The wrapper
is transparent when Passage is invoked as `age-ssh-nt passage`;
[`zsh/integration/interactive-ssh`](../../zsh/integration/interactive-ssh)
provides that alias on SSH connections.

Using `age-plugin-sshagent` directly would make every SSH key a Passage
recipient and require re-encrypting the whole store whenever keys change. This
design pays for one wrapper identity to keep the store recipient stable. The
plugin defines no file layout of its own; the layout below is this project's
convention.

## Commands

| command | purpose |
|---|---|
| `age-ssh-nt bootstrap` | initialize a new box; see [Fresh setup](#fresh-setup) |
| `age-ssh-nt enroll` | add one SSH-agent key without decrypting identities |
| `age-ssh-nt reencrypt` | re-encrypt `identities.age` to all enrolled outer recipients |
| `age-ssh-nt rotate` | begin an inner Passage identity rotation |
| `age-ssh-nt rotate finalize` | verify and finish a pending rotation |
| `age-ssh-nt status` | read-only state and recovery report; see [Checking status](#checking-status) |
| `age-ssh-nt passage <args...>` | run Passage with identities decrypted on demand; `reencrypt` passes through |
| `age-ssh-nt doc` | open this document with `glow`, then `less`, then `cat` |
| `age-ssh-nt help` | command usage |

Lifecycle failures point to `age-ssh-nt status`. Argument errors and deliberate
user aborts do not add that hint.

## State and paths

| setting | resolution |
|---|---|
| `PASSAGE_DIR` | defaults to `~/.passage/store`; its recipient list is `.age-recipients` |
| explicit `AGE_SSH_NT_HOME` | used exactly as provided |
| unset `AGE_SSH_NT_HOME` with an existing directory | first existing directory under `${XDG_DATA_HOME:-$HOME/.local/share}`: `dfs-rhc/age-ssh-nt`, then `age-ssh-nt`, then legacy `~/.age/age-ssh-nt` |
| unset `AGE_SSH_NT_HOME` without state | `$XDG_DATA_HOME/dfs-rhc/age-ssh-nt`, or `~/.local/share/dfs-rhc/age-ssh-nt` when `XDG_DATA_HOME` is unset |

`$AGE_SSH_NT_HOME` is a plain directory created by bootstrap:

| file | secret? | purpose |
|---|---|---|
| `identities.age` | yes (0600) | Passage identities encrypted to enrolled outer recipients |
| `identities.age.recipients-sha256` | no | exact canonical `authorized_recipients` set used by `identities.age` |
| `identities.age.rotation.bak` | yes (0600) | previous encrypted Passage identities retained during rotation |
| `authorized_recipients` | no | one SSH-derived `age1...` recipient per enrolled key |
| `*.identity` | no | plugin fingerprint and salt, useful only with its SSH key in an agent |
| `store_recipient` | no | inner public key recorded for agent-free status checks |
| `README.md` | no | portable pointer to `age-ssh-nt doc` |

In steady state `identities.age` is the only secret in this directory. During a
pending rotation, `identities.age.rotation.bak` is also secret. Back up the
current encrypted identities together with `$PASSAGE_DIR`; the store cannot
regenerate them.

Passage owns `$PASSAGE_DIR/.age-recipients`. Bootstrap adds the inner public key
under a managed comment so new entries keep using the identity supplied by the
wrapper. Before age-ssh-nt rewrites an existing valid list, it atomically saves
the non-secret last-known-good list as
`.age-recipients.age-ssh-nt.bak`. Passage ignores this namespaced file. Invalid
current recipients never overwrite a valid backup, and unsafe backup objects
such as directories or symlinks block mutation.

The durable backup has a generated header, is overwritten by the next managed
rewrite, and is safe to delete after status validates the current list.
Restoring it does not duplicate the header on later rewrites. Managed
comment/key blocks are separated from existing content by exactly one blank
line; an empty file receives no leading blank line.

`authorized_recipients` is canonicalized for encryption by removing comments
and blank lines, sorting, and deduplicating keys; its human-edited source is
never rewritten implicitly. Status reports duplicates. Passage does not
deduplicate `.age-recipients`, so remove duplicate store keys before the one
re-encryption that removes their duplicate ciphertext stanzas.

State-changing commands are serialized by
`$AGE_SSH_NT_HOME/.mutation.lock`. Concurrent mutations report the lock path
and, when its PID file is present, the owning PID. The lock is removed on
normal exit, errors, INT, and TERM. After SIGKILL, confirm that the recorded
process is gone before manually removing a stale lock directory.

## Fresh setup

```sh
age-ssh-nt bootstrap
```

Bootstrap generates a fresh age identity in memory, never saves it as
plaintext, and prints a manifest before changing anything: state directory,
recipients, encrypted identities, sidecars, and Passage recipient update. When
first-key registration is required, the manifest includes the selected SSH key
and a numbered menu appears if several supported keys are available. With
seeded registrations, it lists the existing enrolled keys instead. After
confirmation it encrypts and verifies the new identity. Then verify the store
through the wrapper:

```sh
age-ssh-nt passage show <entry>
```

`identities.age` is written to the sibling temporary file
`identities.age.tmp`, cryptographically verified, and atomically installed. If
interruption occurs after installation
but before `store_recipient`, `README.md`, or the Passage recipient is written,
rerunning bootstrap decrypts the existing file before repairing that metadata.
Incomplete, fabricated, or invalid ciphertext is rejected instead of treated
as a completed bootstrap.

The recipient-set hash is prepared separately; its destination must be a
regular non-symlink file. Hash installation failure restores the prior identity
or removes an incomplete first bootstrap.

If `authorized_recipients` and its corresponding `*.identity` registrations
were seeded from another box, bootstrap skips key registration but still
generates fresh identities and encrypts them to that list. At least one
registered key must be available in the current agent for verification.
Bootstrap never reuses an existing plaintext identity, and generated plaintext
is never retained on disk. Bootstrap includes the first enrollment; add later
devices with `enroll` followed by `reencrypt`.

## Decryption and temporary plaintext

`age-ssh-nt passage` tries each `*.identity` separately because age aborts on
the first plugin identity whose SSH key is unavailable. The agent must hold an
enrolled key locally or be forwarded with `ssh -A`.

Passage needs a seekable identity because interactive commands use stdin and
`passage edit` decrypts the same identity twice. The decrypted identity is a
private 0600 temporary file with platform-specific handling:

| platform | Passage identity path | cleanup and failure behavior |
|---|---|---|
| Linux | open as fd 3, unlink immediately, use `/dev/fd/3`; each age open starts at offset zero | kernel reclaims the anonymous inode on normal exit and SIGKILL |
| macOS/BSD | keep the named file because fdescfs `/dev/fd/N` shares offsets | EXIT/INT/TERM cleanup removes it; SIGKILL can leave it until temporary-directory cleanup |

Every platform uses `XDG_RUNTIME_DIR` when set, otherwise `$TMPDIR` when set,
otherwise `/tmp`; failure in the selected location is fatal rather than retried
elsewhere. `$TMPDIR` and `/tmp` may be disk-backed. Linux immediately unlinks
the file; macOS/BSD retains its 0600 name until cleanup.

## Enrolling a device

Enrollment is purely additive: it never decrypts or rewrites existing
identities and cannot affect previously enrolled devices.

> **Warning:** forward an agent only to a trusted host. The host can request
> the deterministic derivation signature and retain the resulting age private
> key permanently. Agent confirmation approves the request but cannot prevent
> retention of the signature or derived key.

1. From the new device, connect with `ssh -A`, then run `age-ssh-nt enroll`.
   It selects an ordinary `ssh-ed25519` agent key, presents a numbered menu when
   several are loaded, derives a name from its comment, asks for confirmation,
   and appends its outer recipient. Bootstrap must already exist.
2. From any session whose agent holds an enrolled key, run
   `age-ssh-nt reencrypt` to encrypt `identities.age` to the complete outer set.

The pinned plugin excludes RSA, ECDSA, and FIDO/security-key variants. An agent
with no compatible key receives an explicit error.

If interruption creates the non-secret `*.identity` before appending its
recipient, rerunning enrollment with the same key preserves that identity and
appends its missing recipient rather than generating another. Identities are
generated in unique sibling directories and installed atomically. If the
comment-derived filename is claimed while confirmation is pending, enrollment
stops without changing the installed identity and asks the user to rerun so a
new derived name can be selected.

The agent must sign deterministically. A non-deterministic agent produces the
explicit error `agent produced non-deterministic signatures... this key cannot
be used`. This depends on the agent, not only the key: the same key can fail
through one agent and succeed through another.

## Rotating the identities

Rotation replaces only the inner Passage identity. The outer SSH side
(`authorized_recipients` and `*.identity`) remains unchanged, so enrolled
devices continue working:

```sh
age-ssh-nt rotate
```

### Rotation phases

1. **Preflight:** print a manifest of the principal operations and offer an
   optional read-only check that decrypts every store file to `/dev/null`.
   Failures are listed with `age-ssh-nt passage` guidance but do not block
   rotation; final confirmation controls whether to proceed.
2. **Prepare:** restore the exact old store recipient if missing, add the new
   recipient, and verify both before running Passage.
3. **Re-encrypt:** when store files exist, run one Passage pass to old + new;
   otherwise skip it. Because `passage reencrypt` is not assumed atomic, retain
   the old recipient and rename the encrypted old identities to
   `identities.age.rotation.bak`. No plaintext backup is written.
4. **Pending:** status reports the encrypted backup and refuses another
   rotation until finalization.

Recipient changes use an atomic sibling rewrite after refreshing the durable
`.age-recipients.age-ssh-nt.bak`. Rotation also keeps a separate temporary
snapshot and restores it when verification, Passage, or identity installation
fails. SIGKILL can leave the snapshot and pending marker; status reports the
snapshot path and whether installation occurred.

### Finalization

From a session whose agent holds an enrolled key:

```sh
age-ssh-nt rotate finalize
```

Finalization decrypts the current and backup identities to derive their exact
recipients instead of trusting editable marker comments. It refuses unless
every store file decrypts with the current identity, then prints a manifest and
asks for explicit confirmation. It atomically removes the old recipient and
rotation note, restores the standard managed comment immediately above the
current recipient, runs Passage re-encryption when store files exist, and
verifies every store file again. Only then is `identities.age.rotation.bak`
removed. Catchable error paths restore the recipient list and retain the
encrypted backup; SIGKILL may instead leave the temporary snapshot described
above.

If an entry works only with the old identity, restore it before troubleshooting:

```sh
mv "$AGE_SSH_NT_HOME/identities.age" "$AGE_SSH_NT_HOME/identities.age.failed-rotation"
mv "$AGE_SSH_NT_HOME/identities.age.rotation.bak" "$AGE_SSH_NT_HOME/identities.age"
age-ssh-nt reencrypt   # also restores the store_recipient status sidecar
```

Inner rotation does not revoke the outer SSH-derived wrapper. Anyone who can
make the agent sign, or retained the deterministic signature for that plugin
salt, can still derive the wrapper key and decrypt newly rotated
`identities.age`. Revoking that capability requires replacing the plugin
identity and recipient, intentionally outside `rotate`'s scope.

## Checking status

```sh
age-ssh-nt status
```

Status is strictly read-only and reports evidence separately from conclusions:

| check | evidence and result |
|---|---|
| exact outer recipient set | compare the recorded sorted/deduplicated hash with `authorized_recipients`; known one-stanza native/SSH types add a count cross-check, while custom counts may vary; same-count replacement is detected, and `reencrypt` heals a missing hash |
| identities validity | with a matching agent key, decrypt `identities.age`; matching fingerprint plus failed decryption is invalid/plugin failure, while no matching agent is `cannot verify`, never inferred validity |
| store stanza state | require a closed recipient header; compare known one-stanza native/SSH counts with `.age-recipients`, but report custom counts as non-comparable because one recipient may emit multiple stanzas; malformed, truncated, or scrypt-only files are invalid |
| recipient backup safety | validate current recipients and `.age-recipients.age-ssh-nt.bak`; unsafe objects block mutation, and a valid backup is suggested when current state is missing or invalid |
| real inner key | match the actual decrypted `age1...` key, or `store_recipient` without an agent, line by line; comments never prove presence |
| store readability | when possible, decrypt every store file with the inner identity and report `store readable: yes`; distinguish no agent, dead socket, and no matching key instead of guessing |

When synchronized, output ends with `all set - nothing pending`. Otherwise the
`pending:` list names pending checks and recovery actions. Use status whenever
one device cannot decrypt something another device can.

## Recovery index

| symptom | recovery |
|---|---|
| mutation lock remains after SIGKILL | confirm its recorded PID is gone, then remove the lock directory |
| partial enrollment | rerun `age-ssh-nt enroll` with the same key |
| missing or invalid `.age-recipients` | follow status guidance to the namespaced durable backup |
| interrupted recipient transaction | follow status guidance for the temporary snapshot path |
| pending rotation | run `age-ssh-nt rotate finalize` after checking store access |
| old-only store entry | use the exact rollback commands in [Finalization](#finalization) |
| missing bootstrap metadata | rerun `age-ssh-nt bootstrap` with an enrolled key available |
| missing recipient-set hash | run `age-ssh-nt reencrypt` with an enrolled key available |
