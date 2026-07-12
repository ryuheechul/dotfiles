This is not a specific tool but rather a place to store common things across agents.

[AGENTS.md](AGENTS.md) is the single source of instructions shared by multiple agents. Other tools don't duplicate its content - they symlink their own config file straight to it, which mise then deploys to the tool's real config location via the `[dotfiles]` table in [mise/home/conf.d/20-dotfiles-symlinks.toml](../mise/home/conf.d/20-dotfiles-symlinks.toml):

- claude: [../claude/CLAUDE.md](../claude/CLAUDE.md) -> `../agents/AGENTS.md`
- opencode: [../opencode/AGENTS.md](../opencode/AGENTS.md) -> `../agents/AGENTS.md`
- pi: [../pi/agent/AGENTS.md](../pi/agent/AGENTS.md) -> `../../agents/AGENTS.md`

To wire up a new tool, symlink its config file here too instead of copying content.
