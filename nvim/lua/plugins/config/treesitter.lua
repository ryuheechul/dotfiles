-- Treesitter configuration

return function()
  -- `syntax off` should be unnecessary thanks to `additional_vim_regex_highlighting` at ../plugins/config/treesitter.lua
  -- however I'm experiencing that to be not reliable at the time being
  -- hence the extra step
  vim.cmd [[ syntax off ]]

  -- fallback when there is no known treesitter support that exists
  for _, fallback_pair in ipairs {
    -- { 'fallback', 'absence' }
    { 'bash', 'zsh' },
    { 'make', 'Earthfile' },
    { 'toml', 'editorconfig' }, -- I guess it's close enough...
  } do
    vim.treesitter.language.register(fallback_pair[1], fallback_pair[2])
  end

  require('nvim-treesitter.configs').setup {
    ensure_installed = {
      'astro',
      'bash',
      'c',
      'clojure',
      'cmake',
      'comment',
      'commonlisp',
      'cpp',
      'css',
      'dart',
      'diff',
      'dockerfile',
      'dot',
      -- a possible related issue - https://github.com/nvim-treesitter/nvim-treesitter/issues/2913
      -- 'elixir', -- disable for now to deal with slowness with treesitter
      'elm',
      'erlang',
      'fennel',
      'fish',
      'git_config',
      'git_rebase',
      'gitattributes',
      'gitcommit',
      'gitignore',
      'go',
      'graphql',
      'haskell',
      'hcl',
      'hjson',
      'html',
      'http',
      'hurl',
      'java',
      'javascript',
      'jsdoc',
      'json',
      'json5',
      'jsonc',
      'julia',
      'kdl',
      'kotlin',
      'llvm',
      'lua',
      'make',
      'markdown',
      'markdown_inline',
      'nix',
      'ocaml',
      'python',
      'ql',
      'query', -- Tree-sitter query language
      'r',
      'regex',
      'rego',
      'rst',
      'ruby',
      'rust',
      'scala',
      'scheme',
      'scss',
      'solidity',
      'ssh_config',
      'svelte',
      'swift',
      'sql',
      'terraform',
      'tmux',
      'todotxt',
      'toml',
      'tsx',
      'typescript',
      'unison',
      'vim',
      'vimdoc',
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
      -- map keys starting with leader `\` instead of `g` to avoid conflict with my other keys at ../keymaps.lua
      keymaps = {
        init_selection = '\\tnn',
        node_incremental = '\\trn',
        scope_incremental = '\\trc',
        node_decremental = '\\trm',
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
