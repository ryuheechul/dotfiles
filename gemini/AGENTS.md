## Gemini Added Memories
- Do not run `git commit` unless explicitly asked by the user:
  - When constructing commit messages or any multi-line string for shell commands, ensure newlines are correctly preserved. Avoid using backticks (`) for code snippets within commit messages as they can interfere with shell parsing; prefer single quotes (') or double quotes (") for the entire message, and escape internal quotes if necessary.

- Do not run `git restore .` nor `git checkout .`.
- Prioritize and respect external modifications to the codebase; do not overwrite them with previously cached or 'stale' information.
- If there are staged or unstaged Git changes at startup, attempt to infer the ongoing work to avoid redundant explanations from the user.
- Show me the diff (not necessarily `git diff`) after each successful improvements if applicable
- Do not use Serena MCP if it's already activated in some other session
- When a file (within the project) is referenced in a markdown file, link it with markdown syntax too
  - If pointing to a directory that contains README.md, try to link to the directory itself instead of pointing to the README.md directly whenever possible
