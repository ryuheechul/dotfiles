-- Treesitter configuration

return function()
  require('nvim-treesitter.configs').setup {
    ensure_installed = {
      'bash',
      'c',
      'clojure',
      'cmake',
      'comment',
      'commonlisp',
      'cpp',
      'css',
      'dart',
      'dockerfile',
      'dot',
      -- a possible related issue - https://github.com/nvim-treesitter/nvim-treesitter/issues/2913
      -- 'elixir', -- disable for now to deal with slowness with treesitter
      'elm',
      'erlang',
      'fennel',
      'fish',
      'go',
      'graphql',
      'haskell',
      'hcl',
      'hjson',
      'html',
      'http',
      'java',
      'javascript',
      'jsdoc',
      'json',
      'json5',
      'jsonc',
      'julia',
      'kotlin',
      'llvm',
      'lua',
      'make',
      'markdown',
      'nix',
      'ocaml',
      'python',
      'ql',
      'query', -- Treesitter query language
      'r',
      'rego',
      'rst',
      'ruby',
      'rust',
      'scala',
      'scheme',
      'scss',
      'solidity',
      'svelte',
      -- 'swift',
      'todotxt',
      'toml',
      'tsx',
      'typescript',
      'vim',
      'vue',
      'yaml',
    },
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
    textsubjects = {
      enable = true,
      keymaps = {
        ['.'] = 'textsubjects-smart',
        [';'] = 'textsubjects-container-outer',
        ['i;'] = 'textsubjects-container-inner',
      },
    },
  }
end

-- vim: ts=2 sts=2 sw=2 et
