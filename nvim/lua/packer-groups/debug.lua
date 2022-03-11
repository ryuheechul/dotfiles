-- language debugging tools

return {
  'rafcamlet/nvim-luapad', -- `:Luapad` to open a pad and now easily see the result of code in the pad
  {
    'folke/trouble.nvim', -- `:Trouble` to show current troubles in the buffer
    -- these are actually optional but I require to get benefits of them
    requires = {
      'kyazdani42/nvim-web-devicons',
      'folke/lsp-colors.nvim',
    },
    config = function()
      require('trouble').setup {
        -- your configuration comes here
        -- or leave it empty to use the default settings
        -- refer to the configuration section below
      }
    end,
  },
  { 'michaelb/sniprun', run = 'bash ./install.sh' },
}

-- vim: ts=2 sts=2 sw=2 et
