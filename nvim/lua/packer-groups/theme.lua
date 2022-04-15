-- basically colorschemes

-- because of the nature of switching color schems, there will be unused ones listed but commented

return {
  'joshdick/onedark.vim', -- Theme inspired by Atom
  {
    'overcache/NeoSolarized',
    config = function()
      local handle = io.popen 'current-base16'
      local result = handle:read '*a'
      local theme_base = (result:gsub('solarized--', ''))
      handle:close()

      -- -- although below "works" it doesn't work well with my cached base16 shell eval - look at the end of ./shell_ext
      -- local theme_base = (vim.env.BASE16_THEME:gsub('solarized--', ''))

      vim.cmd([[
  colorscheme NeoSolarized
  set background=]] .. theme_base)
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
