-- LSP settings

return function(setup_default, node_root)
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

  local node_root_dir, is_node_repo = node_root()

  local setup_ts_ls = merge(setup_default, {
    root_dir = node_root_dir,
    autostart = is_node_repo and true or false,
  })

  local setup_eslint = merge(setup_default, {
    -- thanks to https://neovim.discourse.group/t/is-it-possible-to-disable-lsp-in-node-modules-directory-file/444/5
    root_dir = function(filename)
      if string.find(filename, 'node_modules/') then
        return nil
      end
      return require('lspconfig.configs.eslint').default_config.root_dir(filename)
    end,
    -- run `EslintFixAll` on save via autocmd, so exclude 'eslint' from lsp-format from ./lsp.lua
    on_attach = function(client, bufnr)
      setup_default.on_attach(client, bufnr)
      vim.api.nvim_create_autocmd('BufWritePre', {
        buffer = bufnr,
        command = 'EslintFixAll',
      })
    end,
    -- or use `:EslintFixAll` to fix all manually - doesn't kick off automatically with format (on save)
  })

  local setup_denols = merge(setup_default, {
    root_dir = lspconfig.util.root_pattern('deno.json', 'deno.jsonc', 'deno.lock'),
    autostart = not is_node_repo and true or false,
  })

  local setup_lua_ls = merge(setup_default, {
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
        workspace = {
          -- to make it stop asking me about this and that
          checkThirdParty = false,
        },
      },
    },
  })

  -- moved on to nixd below
  -- local setup_nil_ls = merge(setup_default, {
  --   settings = {
  --     ['nil'] = {
  --       formatting = {
  --         command = { 'nixfmt' },
  --       },
  --     },
  --   },
  -- })

  -- https://discourse.nixos.org/t/nixd-nix-language-server/28910/40
  local setup_nixd = merge(setup_default, {
    settings = {
      nixd = {
        formatting = {
          command = { 'nixfmt' },
        },
        options = {
          home_manager = {
            expr = '(import <home-manager/modules> { configuration = ~/.config/home-manager/home.nix; pkgs = import <nixpkgs> {}; }).options',
          },
        },
      },
    },
  })
  local setup_bashls = merge(setup_default, {})
  local setup_harper_ls = merge(setup_default, {})
  local setup_nimls = merge(setup_default, {})
  local setup_gopls = merge(setup_default, {})
  local setup_svelte = merge(setup_default, {})
  local setup_astro = merge(setup_default, {
    -- language server is installed via mason at ./lsp.lua
  })
  local setup_tailwindcss = merge(setup_default, {})
  local setup_cssls = merge(setup_default, {})
  local setup_dockerls = merge(setup_default, {})
  local setup_ruby_lsp = merge(setup_default, {})

  local setup_yamlls = merge(setup_default, {
    settings = {
      yaml = {
        format = { enable = true, singleQuote = true },
        validate = true,
        hover = true,
        completion = true,
        schemaStore = {
          enable = true,
          url = 'https://www.schemastore.org/api/json/catalog.json',
        },
      },
    },
  })

  local setup_sourcekit = merge(setup_default, {
    capabilities = {
      workspace = {
        didChangeWatchedFiles = {
          dynamicRegistration = true,
        },
      },
    },
  })
  local setup_sqlls = merge(setup_default, {
    on_attach = function(client, bufnr)
      require('sqls').on_attach(client, bufnr)
      setup_default.on_attach(client, bufnr)
    end,
  })

  local setup_jsonls = merge(setup_default, {})
  --Enable (broadcasting) snippet capability for completion
  setup_jsonls.capabilities.textDocument.completion.completionItem.snippetSupport = true

  local setup_ruff = merge(setup_default, {})
  -- requires ucm to be running already, in case it runs after the editor (after LSP failed)
  -- run it again by `:LspStart`
  local setup_unison = merge(setup_default, {})

  -- determine if it's with container or localhost
  -- read more on that at ../../../remote-container.md
  local gen_setup_pyright = function()
    local volume_name = vim.env.LSP_PYRIGHT_ADDITIONAL_VOLUME
    local path_in_container = vim.env.LSP_PYRIGHT_VOLUME_CONTAINER_PATH

    local setup_pyright = merge(setup_default, {
      settings = {
        python = {
          analysis = {
            diagnosticMode = 'openFileOnly',
          },
        },
      },
    })

    -- in case with no container use
    if not volume_name and not path_in_container then
      return setup_pyright
    end

    -- to be able to go to a definitions for third party packages, mount the volume from code running container
    -- assuming this volume is prepared - read more from ../../../remote-container.md
    local mount = 'type=volume,src=' .. volume_name .. ',dst=' .. path_in_container .. ',readonly'

    -- custom config based on https://github.com/lspcontainers/lspcontainers.nvim#advanced-configuration
    local cmd = require('lspcontainers').command('pyright', {
      -- to achieve an additional "volume mount" (with --mount) as well as "bind mount" (with --volume)
      cmd_builder = function(runtime, volume, image)
        return {
          runtime,
          'container',
          'run',
          '--interactive',
          '--rm',
          '--network=none',
          '--workdir=' .. volume,
          -- pyright container would mount the source code directory as the same path with the one from host
          '--volume='
            .. volume
            .. ':'
            .. volume
            .. ':z',
          '--mount=' .. mount,
          -- mount,
          image,
        }
      end,
    })

    return merge(setup_pyright, {
      -- based on the example from https://github.com/lspcontainers/lspcontainers.nvim#pyright
      before_init = function(params)
        params.processId = vim.NIL
      end,

      cmd = cmd,
      root_dir = require('lspconfig/util').root_pattern('.git', vim.fn.getcwd()),
    })
  end

  -- using efm to replace null-ls
  local setup_efm = merge(
    setup_default,
    (function()
      -- Register linters and formatters per language
      -- local eslint = require 'efmls-configs.formatters.eslint'
      -- local denofmt = require 'efmls-configs.formatters.deno_fmt'
      local stylua = require 'efmls-configs.formatters.stylua'

      -- local ts_formatter = is_node_repo and eslint or denofmt

      local languages = {
        -- NOTE: disable eslint as it's problematic, now rely on setup_eslint from ./lsp-servers.lua
        -- typescript = { ts_formatter },
        -- typescriptreact = { ts_formatter },
        lua = { stylua },
      }

      return {
        filetypes = vim.tbl_keys(languages),
        settings = {
          rootMarkers = { '.git/' },
          languages = languages,
        },
        init_options = {
          documentFormatting = true,
          documentRangeFormatting = true,
        },
      }
    end)()
  )

  -- Enable the following language servers with `setup_default`
  return {
    clangd = setup_default,
    rust_analyzer = setup_default,
    pyright = gen_setup_pyright(),
    harper_ls = setup_harper_ls,
    ts_ls = setup_ts_ls,
    denols = setup_denols,
    lua_ls = setup_lua_ls,
    -- nil_ls = setup_nil_ls,
    nixd = setup_nixd,
    eslint = setup_eslint,
    sqlls = setup_sqlls,
    sourcekit = setup_sourcekit,
    jsonls = setup_jsonls,
    nimls = setup_nimls,
    gopls = setup_gopls,
    svelte = setup_svelte,
    astro = setup_astro,
    cssls = setup_cssls,
    tailwindcss = setup_tailwindcss,
    yamlls = setup_yamlls,
    dockerls = setup_dockerls,
    ruby_lsp = setup_ruby_lsp,
    efm = setup_efm,
    ruff = setup_ruff,
    unison = setup_unison,
    bashls = setup_bashls,
  }
end

-- vim: ts=2 sts=2 sw=2 et
