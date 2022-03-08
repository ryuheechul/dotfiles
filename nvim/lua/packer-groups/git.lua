-- git related

return {
  'tpope/vim-fugitive', -- Git commands in nvim
  'tpope/vim-rhubarb', -- Fugitive-companion to interact with github
  -- Add git related info in the signs columns and popups
{ 'lewis6991/gitsigns.nvim',
    requires = { 'nvim-lua/plenary.nvim' },
    config = function()
      require'gitsigns'.setup {
        signs = {
          add = { hl = 'GitGutterAdd', text = '+' },
          change = { hl = 'GitGutterChange', text = '~' },
          delete = { hl = 'GitGutterDelete', text = '_' },
          topdelete = { hl = 'GitGutterDelete', text = '‾' },
          changedelete = { hl = 'GitGutterChange', text = '~' },
        },
      }
    end
  },
}

-- vim: ts=2 sts=2 sw=2 et
