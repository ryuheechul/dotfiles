# Added Memories for Agents

## Git Operations
- Do not execute `git commit` unless explicitly instructed.
  - When constructing commit messages or any multi-line string for shell commands, ensure newlines are correctly preserved. Avoid using backticks (`) for code snippets within commit messages as they can interfere with shell parsing; prefer single quotes (') or double quotes (") for the entire message, and escape internal quotes if necessary.
- Do not run `git restore .` nor `git checkout .`.
- Do not run or suggest `git add` unless explicitly instructed.
- Provide a summary of changes (e.g., a diff or clear explanation) after each successful improvement, if applicable.
- Commit messages describe the resulting diff only, not the session's editing journey to get there (e.g. a file that was edited then reverted before ever being committed, or code that moved from one file to another mid-session). That history isn't in the diff and reading the message shouldn't require having watched the session. Keep messages concise - skip narrating verification/testing steps performed along the way unless the outcome is essential to understanding the change.

## Documentation
- Use Markdown link syntax whenever a project file is referenced in a Markdown document.
  - If pointing to a directory that contains README.md, link to the directory itself instead of pointing to the README.md directly whenever possible.
- When commenting in YAML, prefer placing the comment on the line above the target field rather than as a trailing comment on the right side.

## Updating Code
- Prioritize and respect external modifications to the codebase; do not overwrite them with previously cached or 'stale' information.
- Don't remove comments arbitrarily, especially when they are not relevant to or requested for the current task.
- When introducing new components that integrate with existing logic, verify their behavior in isolation (e.g., using dedicated inputs and outputs) before integration.
  - Create a temporary file for this if necessary.
  - See if there is a skill that describes this concept better.
- Develop new logic or complex solutions in temporary files first; only apply them to the codebase after they have been thoroughly verified.
