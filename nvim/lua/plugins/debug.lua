-- language debugging tools

return {
  { -- `:Luapad` to open a pad and now easily see the result of code in the pad
    'rafcamlet/nvim-luapad',
    event = 'VeryLazy',
  },
  { -- `:Trouble` to show current troubles in the buffer
    'folke/trouble.nvim',
    -- since `v3.*` is not very usable for me yet
    commit = 'a8264a65a0b894832ea642844f5b7c30112c458f',
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
      severity = vim.diagnostic.severity.ERROR, -- use `s` to toggle between levels
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
      { -- A UI for nvim-dap
        'rcarriga/nvim-dap-ui',
        dependencies = {
          'nvim-neotest/nvim-nio', -- A library for asynchronous IO in Neovim
        },
      },
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
    init = function()
      -- how do i write this in lua?
      vim.cmd [[
        let g:conjure#mapping#prefix = "<leader>rc"
      ]]
    end,
  },
  { -- A Fennel REPL that runs in Neovim
    'gpanders/fennel-repl.nvim',
    event = 'VeryLazy',
  },
  { -- A framework for interacting with tests within Neovim.
    'nvim-neotest/neotest',
    event = 'VeryLazy',
    dependencies = {
      'nvim-lua/plenary.nvim',
      'antoinemadec/FixCursorHold.nvim',
      'nvim-treesitter/nvim-treesitter',
      'nvim-neotest/neotest-python',
    },
    config = function()
      require('neotest').setup {
        adapters = {
          -- quickest to way to see how it works
          -- clone - https://github.com/pytest-dev/pytest
          -- setup venv and install packages - https://github.com/pytest-dev/pytest/blob/main/CONTRIBUTING.rst (search `venv`)
          -- locate https://github.com/pytest-dev/pytest/blob/main/testing/example_scripts/doctest/main_py/test_normal_module.py locally
          -- run `:Neotest run`
          require 'neotest-python',
        },
      }
    end,
  },
}

-- vim: ts=2 sts=2 sw=2 et
