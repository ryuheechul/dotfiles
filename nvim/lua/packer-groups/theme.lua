-- basically colorschemes

-- because of the nature of switching color schems, there will be unused ones listed but commented

return {
  'joshdick/onedark.vim', -- Theme inspired by Atom
  {
    'overcache/NeoSolarized',
    config = function()
      vim.cmd [[
  colorscheme NeoSolarized
  set background=light
]]
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
