-- helper to run system command
-- super simple for now
local run_cmd = function(cmd)
  local handle = io.popen(cmd .. ' 2>/dev/null')

  if handle ~= nil then
    local result = handle:read '*a'
    handle:close()

    return result
  end
  return ''
end

-- usage: require('utils.shell').run_cmd 'which pwd'
return {
  run_cmd = run_cmd,
  -- usage: require('utils.shell').grep 'lua' '~/dotfiles'
  grep = function(pattern, filename)
    local result = run_cmd('rg ' .. pattern .. ' ' .. filename)
    return result
  end,
}
