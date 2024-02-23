-- basically colorschemes

-- because of the nature of switching color schems, there will be unused ones listed but commented

-- this function seems to be global in order to be called from below
-- I could wrap the whole thing in a function to have a closure effect
-- but I'm lazy and why not have this function to be available in command line as well
local follow_base16_shell = function()
  -- instead of relying vim.env.BASE16_THEME
  -- `current-base16` response the most accurate value
  local handle = io.popen 'current-base16'

  if handle ~= nil then
    local result = handle:read '*a'
    local theme_base = (result:gsub('solarized--', ''))
    handle:close()

    vim.api.nvim_set_option('background', theme_base)
  end
end

return {
  {
    -- NeoSolarized: A fixed solarized colorscheme for better truecolor support.
    'JoveYu/NeoSolarized', -- using fork instead for this issue, https://github.com/overcache/NeoSolarized/issues/26
    lazy = false,
    priority = 900,
    dependencies = {
      'rktjmp/fwatch.nvim',
      {
        'neanias/everforest-nvim',
        lazy = false,
        priority = 1000,
      },
    },
    init = function()
      -- set a theme first
      if vim.env.my_nvim_theme_solarized ~= nil then
        vim.cmd [[ colorscheme NeoSolarized ]]
      else
        vim.cmd [[ colorscheme everforest ]]
      end

      -- comply with base16
      follow_base16_shell()

      -- set up a callback on file change so it can correct the theme tone by itself
      local fwatch = require 'fwatch'
      -- because `~` wouldn't work here
      fwatch.watch(vim.env.HOME .. '/.base16_theme.updated-time', {
        on_event = function()
          -- use `defer_fn` to avoid [blahblah] must not be called in a lua loop callback
          vim.defer_fn(function()
            follow_base16_shell()
          end, 1)
        end,
      })
    end,
  },
  -- {
  --   'shaunsingh/solarized.nvim',
  --   -- wait until this plugin provide dark theme
  --   config = function()
  --     require('solarized').set()
  --   end,
  -- },
  --   {
  --     'savq/melange',
  --     config = function()
  --       vim.cmd [[
  --   colorscheme melange
  --   set background=light
  -- ]]
  --     end,
  --   },
}

-- vim: ts=2 sts=2 sw=2 et
