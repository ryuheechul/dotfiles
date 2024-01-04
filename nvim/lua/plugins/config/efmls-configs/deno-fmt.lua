-- Metadata
-- languages: javascript,typescript
-- url: https://docs.deno.com/runtime/manual/tools/formatter

local fs = require 'efmls-configs.fs'

local formatter = 'deno'
-- using ${FILEEXT} thanks to https://github.com/mattn/efm-langserver/pull/115/files
local args = 'fmt - --ext ${FILEEXT}'

local command = string.format('%s %s', fs.executable(formatter, nil), args)

return {
  formatCommand = command,
  formatStdin = true,
}

-- note on this file: using this until there is a upstream version
