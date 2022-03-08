-- Treesitter configuration

return function()
  require('nvim-treesitter.configs').setup {
    ensure_installed = 'maintained',
    sync_install = false,
    highlight = {
      enable = true, -- false will disable the whole extension
      additional_vim_regex_highlighting = false,
    },
    incremental_selection = {
      enable = true,
      keymaps = {
        init_selection = 'gnn',
        node_incremental = 'grn',
        scope_incremental = 'grc',
        node_decremental = 'grm',
      },
    },
    indent = {
      enable = true,
    },
    textobjects = {
      select = {
        enable = true,
        lookahead = true, -- Automatically jump forward to textobj, similar to targets.vim
        keymaps = {
          -- You can use the capture groups defined in textobjects.scm
          ['af'] = '@function.outer',
          ['if'] = '@function.inner',
          ['ac'] = '@class.outer',
          ['ic'] = '@class.inner',
          ['a,'] = '@parameter.outer',
          ['i,'] = '@parameter.inner',
        },
      },
      move = {
        enable = true,
        set_jumps = true, -- whether to set jumps in the jumplist
        goto_next_start = {
          [']m'] = '@function.outer',
          [']]'] = '@class.outer',
          ['],'] = '@parameter.inner',
        },
        goto_next_end = {
          [']M'] = '@function.outer',
          [']['] = '@class.outer',
          [']<'] = '@parameter.outer',
        },
        goto_previous_start = {
          ['[m'] = '@function.outer',
          ['[['] = '@class.outer',
          ['[,'] = '@parameter.inner',
        },
        goto_previous_end = {
          ['[M'] = '@function.outer',
          ['[]'] = '@class.outer',
          ['[<'] = '@parameter.outer',
        },
      },
    },
    -- this is for https://github.com/andymass/vim-matchup#tree-sitter-integration
    matchup = {
      enable = true, -- mandatory, false will disable the whole extension
    },
    -- for p00f/nvim-ts-rainbow
    rainbow = {
      enable = true,
    },
  }
end

-- vim: ts=2 sts=2 sw=2 et
