-- language debugging tools

return {
  'rafcamlet/nvim-luapad', -- `:Luapad` to open a pad and now easily see the result of code in the pad
  {
    'folke/trouble.nvim', -- `:Trouble` to show current troubles in the buffer
    -- these are actually optional but I require to get benefits of them
    dependencies = {
      'nvim-tree/nvim-web-devicons',
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
  {
    'michaelb/sniprun',
    build = 'bash ./install.sh',
    lazy = true,
    -- until there is more update on https://github.com/michaelb/sniprun/issues/54
    cond = function()
      return vim.fn.has 'linux' == 1
    end,
  },
  -- maybe one day this becomes useful
  -- { 'rcarriga/vim-ultest', dependencies = { 'vim-test/vim-test' }, build = ':UpdateRemotePlugins' },
  {
    'weilbith/nvim-code-action-menu', -- enables `:CodeActionMenu`
    cmd = 'CodeActionMenu',
  },
  { -- Debug Adapter Protocol client implementation for Neovim
    'mfussenegger/nvim-dap',
    dependencies = {
      'anuvyklack/keymap-layer.nvim', -- Create a key layer in Neovim
      'jbyuki/one-small-step-for-vimkind', -- Debug adapter for Neovim lua files (including plugins)
      'rcarriga/nvim-dap-ui', -- A UI for nvim-dap
      -- adds virtual text support to nvim-dap. nvim-treesitter is used to find variable definitions.
      'theHamsta/nvim-dap-virtual-text',
      { -- nvim-dap adapter for vscode-js-debug
        'mxsdev/nvim-dap-vscode-js',
        dependencies = {
          { -- The VS Code JavaScript debugger
            'microsoft/vscode-js-debug',
            lazy = true, -- since this is not a real plugin
            version = 'v1.74.1',
            -- since it's not on npm registry, building directly borrowing power of nix-shell to satisfy dev dependencies
            build = "nix-shell --command 'npm i --legacy-peer-deps && npm run compile' ~/.config/dfs-rhc/nvim/shell/shell.nix",
          },
        },
      },
    },
    config = require 'plugins.config.dap',
  },
}

-- vim: ts=2 sts=2 sw=2 et
