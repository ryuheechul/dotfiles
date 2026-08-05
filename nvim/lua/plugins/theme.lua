-- basically colorschemes

-- because of the nature of switching color schems, there will be unused ones listed but commented

-- this function seems to be global in order to be called from below
-- I could wrap the whole thing in a function to have a closure effect
-- but I'm lazy and why not have this function to be available in command line as well
local follow_tinty_theme = function()
  -- no env var to rely on: `theme-tone` responds the most accurate value,
  -- and unlike `theme-name` it prints just the tone (light/dark) - theme-name
  -- carries the family's scheme name (solarized-light vs builtin-solarized-light),
  -- so decoding the tone from it would break when the family changes
  local handle = io.popen 'theme-tone'

  if handle ~= nil then
    local tone = handle:read '*l'
    handle:close()

    if tone ~= nil and tone ~= '' then
      vim.api.nvim_set_option('background', tone)
    end
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

      -- comply with the current tinty theme
      follow_tinty_theme()

      -- set up a callback on file change so it can correct the theme tone by itself
      -- (subscriber of "One tone, every layer" - ../../../docs/mechanics.md)
      local fwatch = require 'fwatch'
      -- because `~` wouldn't work here
      fwatch.watch(vim.env.HOME .. '/.active-theme.updated-time', {
        on_event = function()
          -- use `defer_fn` to avoid [blahblah] must not be called in a lua loop callback
          vim.defer_fn(function()
            follow_tinty_theme()
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
