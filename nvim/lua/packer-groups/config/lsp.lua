-- LSP settings

return function()
  local lspformat = require 'lsp-format'
  local navic = require 'nvim-navic'

  lspformat.setup {
    sql = {
      exclude = { 'sqls' }, -- formatting doesn't seem to be very good so excluding for now
    },
  }

  -- Mappings.
  -- See `:help vim.diagnostic.*` for documentation on any of the below functions
  local opts = { noremap = true, silent = true }
  vim.keymap.set('n', '<space>e', vim.diagnostic.open_float, opts)
  vim.keymap.set('n', '[d', vim.diagnostic.goto_prev, opts)
  vim.keymap.set('n', ']d', vim.diagnostic.goto_next, opts)
  vim.keymap.set('n', '<space>q', vim.diagnostic.setloclist, opts)

  -- Use an on_attach function to only map the following keys
  -- after the language server attaches to the current buffer
  local on_attach = function(client, bufnr)
    -- to enable format on save
    lspformat.on_attach(client)

    if client.server_capabilities.documentSymbolProvider then
      navic.attach(client, bufnr)
    end

    -- Enable completion triggered by <c-x><c-o>
    vim.api.nvim_buf_set_option(bufnr, 'omnifunc', 'v:lua.vim.lsp.omnifunc')

    -- Mappings.
    -- See `:help vim.lsp.*` for documentation on any of the below functions

    local bufopts = { noremap = true, silent = true, buffer = bufnr }

    local desc_opts = function(desc)
      return require('utils.table').merge(bufopts, { desc = desc })
    end

    vim.keymap.set('n', 'gD', vim.lsp.buf.declaration, desc_opts 'go to declaration')
    vim.keymap.set('n', 'gd', vim.lsp.buf.definition, desc_opts 'go to definition')
    vim.keymap.set('n', 'K', vim.lsp.buf.hover, desc_opts 'hover')
    vim.keymap.set('n', 'gj', vim.lsp.buf.implementation, desc_opts 'go to implementations')
    vim.keymap.set('n', '<S-k>', vim.lsp.buf.signature_help, desc_opts 'signature help')
    vim.keymap.set('n', '<space>wa', vim.lsp.buf.add_workspace_folder, desc_opts 'add workspace')
    vim.keymap.set('n', '<space>wr', vim.lsp.buf.remove_workspace_folder, desc_opts 'remove workspace')
    vim.keymap.set('n', '<space>wl', function()
      print(vim.inspect(vim.lsp.buf.list_workspace_folders()))
    end, desc_opts 'list workspace')
    vim.keymap.set('n', '<space>D', vim.lsp.buf.type_definition, desc_opts 'type definition')
    vim.keymap.set('n', '<space>rn', vim.lsp.buf.rename, desc_opts 'rename')
    vim.keymap.set('n', '<space>ca', vim.lsp.buf.code_action, desc_opts 'code action')
    vim.keymap.set('n', 'gr', vim.lsp.buf.references, desc_opts 'go to references')

    -- this has nothing to do with format on save
    vim.keymap.set('n', '<space>bf', vim.lsp.buf.format, desc_opts 'format buffer')
  end

  -- nvim-cmp supports additional completion capabilities
  local capabilities = require('cmp_nvim_lsp').default_capabilities()

  local setup_default = {
    on_attach = on_attach,
    capabilities = capabilities,
    flags = {
      -- This will be the default in neovim 0.7+
      debounce_text_changes = 150,
    },
  }

  -- delegate server specific setup to lsp-servers
  local servers = require 'packer-groups.config.lsp-servers' (setup_default)
  -- this is to prevent the old server with stale config to keep running
  -- to play nice with ../../utils/packer-auto-compile.lua
  -- also wrap with `pcall` since the first time there is no this cmd is available.
  pcall(vim.cmd.LspStop)

  local lspconfig = require 'lspconfig'
  for server, setup in pairs(servers) do
    lspconfig[server].setup(setup)
  end
  -- this will allow more complicated lsp capabilities when I want
  -- require('navigator').setup { lsp = servers }

  -- for floating signature hints
  require('lsp_signature').setup {
    floating_window = false, -- to let 'hrsh7th/cmp-nvim-lsp-signature-help' to overtake
  }
end

-- vim: ts=2 sts=2 sw=2 et
