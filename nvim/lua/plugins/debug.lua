-- language debugging tools

return {
  { -- `:Luapad` to open a pad and now easily see the result of code in the pad
    'rafcamlet/nvim-luapad',
    event = 'VeryLazy',
  },
  { -- `:Trouble` to show current troubles in the buffer
    'folke/trouble.nvim',
    -- these are actually optional but I require to get benefits of them
    dependencies = {
      'nvim-tree/nvim-web-devicons',
      'folke/lsp-colors.nvim',
    },
    init = function()
      vim.keymap.set('n', 'gt', ':Trouble<CR>', { noremap = true, silent = true, desc = 'Trouble' })
    end,
    opts = {
      position = 'bottom',
      action_keys = {
        hover = 'gk',
      },
      auto_open = true,
      auto_close = true,
      auto_preview = false, -- this being up could cause the feeling of cursors jumping around with auto_open/close
    },
    event = 'VeryLazy',
  },
  { -- Sniprun is a code runner plugin for neovim written in Lua and Rust
    'michaelb/sniprun',
    build = "np-build-via-nix-shell 'bash ./install.sh'",
    event = 'VeryLazy',
    opts = {
      selected_interpreters = { 'JS_TS_deno' },
      repl_enable = { 'JS_TS_deno' },
    },
  },
  -- maybe one day this becomes useful
  -- { 'rcarriga/vim-ultest', dependencies = { 'vim-test/vim-test' }, build = ':UpdateRemotePlugins' },
  { -- enables `:CodeActionMenu`
    'weilbith/nvim-code-action-menu',
    cmd = 'CodeActionMenu',
  },
  { -- Debug Adapter Protocol client implementation for Neovim
    'mfussenegger/nvim-dap',
    event = 'VeryLazy',
    dependencies = {
      'anuvyklack/keymap-layer.nvim', -- Create a key layer in Neovim
      'jbyuki/one-small-step-for-vimkind', -- Debug adapter for Neovim lua files (including plugins)
      'rcarriga/nvim-dap-ui', -- A UI for nvim-dap
      -- adds virtual text support to nvim-dap. nvim-treesitter is used to find variable definitions.
      'theHamsta/nvim-dap-virtual-text',
      { -- nvim-dap adapter for vscode-js-debug
        'mxsdev/nvim-dap-vscode-js',
        version = '*',
        dependencies = {
          { -- The VS Code JavaScript debugger
            'microsoft/vscode-js-debug',
            lazy = true, -- since this is not a real plugin
            version = 'v1.74.1',
            -- since it's not on npm registry, building directly borrowing power of nix-shell to satisfy dev dependencies
            build = "np-build-via-nix-shell 'npm i --legacy-peer-deps && npm run compile'",
          },
        },
      },
    },
    config = require 'plugins.config.dap',
  },
  { -- Interactive evaluation for Neovim (mostly for lisp languages)
    'Olical/conjure',
    ft = { 'fennel', 'clojure', 'racket', 'lua', 'python', 'rust' },
  },
  { -- A Fennel REPL that runs in Neovim
    'gpanders/fennel-repl.nvim',
    event = 'VeryLazy',
  },
}

-- vim: ts=2 sts=2 sw=2 et
