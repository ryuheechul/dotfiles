-- Treesitter configuration

return function()
  -- `syntax off` should be unnecessary but keeping it for now as a precaution if needed
  vim.cmd [[ syntax off ]]

  -- fallback when there is no known treesitter support that exists
  for _, fallback_pair in ipairs {
    -- { 'fallback', 'absence' }
    { 'bash', 'zsh' },
    { 'make', 'Earthfile' },
    { 'toml', 'editorconfig' }, -- I guess it's close enough...
    { 'hcl', 'alloy' }, -- I guess it's close enough...
    { 'bash', 'dotenv' }, -- I guess it's close enough...
  } do
    vim.treesitter.language.register(fallback_pair[1], fallback_pair[2])
  end

  require('tree-sitter-manager').setup {
    -- List of parsers to install automatically on startup
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
      'nginx',
      'nix',
      'nu',
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

    -- Automatically install missing parsers when opening a new filetype
    auto_install = true,

    -- Enable Tree-sitter highlighting (enabled by default)
    highlight = true,

    -- UI configuration for the :TSManager window
    border = 'rounded',
  }

  -- Use native Neovim indentation
  vim.o.indentexpr = 'v:lua.vim.treesitter.indent()'
end

-- vim: ts=2 sts=2 sw=2 et
