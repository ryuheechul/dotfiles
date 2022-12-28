-- LSP settings

return function(setup_default)
  local merge = require('utils.table').merge

  -- why additional `--tsserver-path`?
  -- get this
  -- even though nvim_lsp use the term `tsserver`,
  -- what it talks to is `typescript-language-server` because tsserver is not a language server.
  -- but typescript-language-server do use `tsserver` binary to function properly
  -- so do you have both binaries and open typescript file in a typescript project?
  -- it seems work fine. However the tricky part about this situation is when you open javascript file.
  -- because I go around and randomly open a stray js file!
  -- suddenly it shows an error message below
  -- and it confuses you because you do have the binary in the $PATH!!
  -- it's because typescript-language-server seems to look for binaries from node_modules but doesn't seem to clearly communicate that
  -- I personally spent some hours to wrap my head around this and
  -- it wasn't so hard to find other people are lost about this
  -- https://github.com/neovim/nvim-lspconfig/issues/143
  -- https://github.com/neovim/nvim-lspconfig/issues/260
  -- https://www.reddit.com/r/neovim/comments/pn1rl7/neovim_lsp_for_javascript_instead_of_typescript/hcm74op/
  -- https://github.com/emacs-lsp/lsp-mode/issues/1380 (even on emacs!)
  -- but this little journey allow me to understand how things are put together little better :)

  --[[
  Error executing vim.schedule lua callback: ...eovim-unwrapped-0.6.1/share/nvim/runtime/lua/vim/lsp.lua:894: RPC[Error]
   code_name = InternalError, message = "Request initialize failed with message: Could not find a valid tsserver executable in the workspace or in the $PATH. Please ensure that the \"typescript\" dependency is installed in either location. Exiting."
  stack traceback:
          [C]: in function 'assert'
          ...eovim-unwrapped-0.6.1/share/nvim/runtime/lua/vim/lsp.lua:894: in function 'cb'
          vim.lua:285: in function <vim.lua:285>
  Example custom server
  Make runtime files discoverable to the server
  ]]

  local lspconfig = require 'lspconfig'

  local setup_tsserver = merge(setup_default, {
    cmd = { 'typescript-language-server', '--stdio', '--tsserver-path', 'tsserver' },
    root_dir = lspconfig.util.root_pattern 'package.json',
  })

  -- use :EslintFixAll to fix all - doesn't kick off automatically with format (on save)
  local setup_eslint = merge(setup_default, {
    root_dir = lspconfig.util.root_pattern 'package.json',
  })

  local setup_denols = merge(setup_default, {
    root_dir = lspconfig.util.root_pattern('deno.json', 'deno.jsonc'),
  })

  local setup_sumneko_lua = merge(setup_default, {
    single_file_support = true,
    settings = {
      Lua = {
        completion = {
          workspaceWord = true,
          callSnippet = 'Both',
        },
        runtime = {
          version = 'LuaJIT',
        },
        diagnostics = {
          -- Get the language server to recognize the `vim` global
          globals = { 'vim', 'require' },
        },
      },
    },
  })

  local setup_rnix = merge(setup_default, {})

  local setup_sqls = merge(setup_default, {
    on_attach = function(client, bufnr)
      require('sqls').on_attach(client, bufnr)
      setup_default.on_attach(client, bufnr)
    end,
  })

  local setup_jsonls = merge(setup_default, {})
  --Enable (broadcasting) snippet capability for completion
  setup_jsonls.capabilities.textDocument.completion.completionItem.snippetSupport = true

  -- Enable the following language servers with `setup_default`
  return {
    clangd = setup_default,
    rust_analyzer = setup_default,
    pyright = setup_default,
    tsserver = setup_tsserver,
    denols = setup_denols,
    sumneko_lua = setup_sumneko_lua,
    rnix = setup_rnix,
    eslint = setup_eslint,
    sqls = setup_sqls,
    jsonls = setup_jsonls,
  }
end

-- vim: ts=2 sts=2 sw=2 et
