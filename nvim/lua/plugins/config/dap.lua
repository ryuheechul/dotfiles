-- DAP settings

return function()
  local nlua = function()
    local dap = require 'dap'

    dap.configurations.lua = {
      {
        type = 'nlua',
        request = 'attach',
        name = 'Attach to running Neovim instance',
      },
    }

    dap.adapters.nlua = function(callback, config)
      ---@diagnostic disable: undefined-field
      callback { type = 'server', host = config.host or '127.0.0.1', port = config.port or 8086 }
    end

    -- for debuggee - Neovim instance A that wants to be debugged
    -- so launch this at the neovim client that is a debuggee which also runs the server that debugger client can attach
    vim.keymap.set('n', '<leader>ro', function()
      require('osv').launch { port = 8086 }
    end, { noremap = true, desc = 'DAP OSV launch' })

    -- for debugger - Neovim instance B that will debug instance A
    -- rely most of dap commands via Telescope instead of keybindings - currently it's mapped to <space>fd
    -- also quick_debug keymap layer supports even quicker interactions with debugger
  end

  local setup_dapui = function()
    local dap, dapui = require 'dap', require 'dapui'
    dapui.setup {
      -- since I don't want repl to be there due to ungraceful workflow I override `layouts` to get rid of it
      layouts = {
        {
          -- You can change the order of elements in the sidebar
          elements = {
            -- Provide IDs as strings or tables with "id" and "size" keys
            { id = 'scopes', size = 0.25 }, -- percent
            { id = 'breakpoints', size = 0.25 },
            { id = 'stacks', size = 0.25 },
            { id = 'watches', size = 0.25 },
          },
          size = 40, -- columns
          position = 'left',
        },
        {
          elements = {
            { id = 'console' },
          },
          size = 10, -- columns
          position = 'bottom',
        },
      },
      controls = {
        -- enabled = vim.fn.exists '+winbar' == 1,
        enabled = true,
        element = 'console',
        icons = {
          pause = '',
          play = '',
          step_into = '',
          step_over = '',
          step_out = '',
          step_back = '',
          run_last = '',
          terminate = '',
        },
      },
    }

    local dapui_event_id = 'my_dapui_config'

    local before = dap.listeners.before
    local after = dap.listeners.after

    after.event_initialized[dapui_event_id] = function(--[[ session, body ]])
      dapui.open()
    end

    after.event_stopped[dapui_event_id] = function()
      dapui.open()
    end

    before.event_terminated[dapui_event_id] = function()
      dapui.close()
    end

    before.event_exited[dapui_event_id] = function()
      dapui.close()
    end

    vim.keymap.set('n', '<leader>ru', dapui.toggle, { noremap = true, desc = 'DAP UI toggle' })

    local dapGrp = vim.api.nvim_create_augroup('MyDAPAUG', { clear = true })
    -- although `quit_debug` could do the same but there is no guarantee so this is an insurance
    vim.api.nvim_create_autocmd('FileType', {
      pattern = { 'dapui_*' },
      callback = function()
        vim.keymap.set('n', 'q', function()
          require('dapui').close()
        end, { noremap = true, silent = true, buffer = true, desc = 'close dap-ui' })
      end,
      group = dapGrp,
    })
  end

  local layer_keys = function()
    local dap = require 'dap'
    local KeyLayer = require 'keymap-layer'
    local m = { 'n' } -- modes

    local quick_debug_kl = KeyLayer {
      enter = {},
      layer = {
        { m, 'p', require('dap.ui.widgets').hover, { noremap = true, desc = 'hover' } },
        { m, 't', dap.toggle_breakpoint, { noremap = true, desc = 'tgl breakpoints' } },
        { m, 'i', dap.step_into, { noremap = true, desc = 'step into' } },
        { m, 'o', dap.step_out, { noremap = true, desc = 'step out' } },
        { m, 'n', dap.step_over, { noremap = true, desc = 'step over' } },
        { m, 'p', dap.step_back, { noremap = true, desc = 'step back' } },
        { m, 'c', dap.continue, { noremap = true, desc = 'continue' } },
        { m, 'q', dap.disconnect, { noremap = true, desc = 'disconnect' } },
      },
      exit = {},
      opts = {
        on_enter = function()
          print 'Enter quick debug layer'
        end,
        on_exit = function()
          print 'Exit quick debug layer'
        end,
      },
    }

    local smart_quit_debug = function()
      local ft = vim.bo.filetype
      if ft:match '^dapui_.*' then
        require('dapui').close()
      else
        dap.disconnect()
      end
    end

    -- to prevent file buffer to close and disconnect dap instead
    local quit_debug_kl = KeyLayer {
      enter = {},
      layer = {
        { m, 'q', smart_quit_debug, { noremap = true, desc = 'disconnect' } },
      },
      exit = {},
      opts = {
        on_enter = function()
          print 'Enter quit debug layer'
        end,
        on_exit = function()
          print 'Exit quit debug layer'
        end,
      },
    }

    local dap_event_id = 'my_dap_kb'

    local after = dap.listeners.after

    after.event_initialized[dap_event_id] = function(--[[ session, body ]])
      quit_debug_kl:activate()
      quick_debug_kl:activate()
    end
    after.event_terminated[dap_event_id] = function()
      quick_debug_kl:exit()
      quit_debug_kl:exit()
    end
    after.event_exited[dap_event_id] = function()
      quick_debug_kl:exit()
      quit_debug_kl:exit()
    end

    after.event_stopped[dap_event_id] = function()
      quick_debug_kl:activate()
    end
    after.event_continued[dap_event_id] = function()
      quick_debug_kl:exit()
    end
  end

  local attach_python = function()
    local dap = require 'dap'
    local host = vim.env.DAP_PYTHON_REMOTE_HOST or 'localhost'
    local port = vim.env.DAP_PYTHON_REMOTE_PORT or '5678'
    local portnr = tonumber(port)

    -- Wherever your Python code lives locally.
    local localRoot = vim.fn.getcwd()
    -- Wherever your Python code lives in the container.
    local remoteRoot = vim.env.DAP_PYTHON_REMOTE_ROOT or '/app'
    local adapter = { type = 'server', host = host, port = port }

    local pathMappings = {
      {
        localRoot = localRoot,
        remoteRoot = remoteRoot,
      },
    }

    local also_packages = vim.env.DAP_PYTHON_LOCAL_ROOT_PACKAGE ~= nil and vim.env.DAP_PYTHON_REMOTE_ROOT_PACKAGE ~= nil

    if also_packages then
      table.insert(pathMappings, {
        localRoot = vim.env.DAP_PYTHON_LOCAL_ROOT_PACKAGE,
        remoteRoot = vim.env.DAP_PYTHON_REMOTE_ROOT_PACKAGE,
      })
    end

    -- Except the ones that has special meaning to nvim-dap, the rest will be passed directly to adapters. Visit the links below
    -- https://github.com/mfussenegger/nvim-dap/blob/6f2ea9e33b48a51849ec93c6c38148a5372018e4/lua/dap/session.lua#L1519
    -- https://github.com/mfussenegger/nvim-dap/blob/6f2ea9e33b48a51849ec93c6c38148a5372018e4/lua/dap/session.lua#L1452
    local config = {
      type = 'python',
      request = 'attach',
      connect = {
        port = portnr,
        host = host,
      },
      mode = 'remote',
      name = 'Remote Attached Debugger',
      cwd = vim.fn.getcwd(),
      pathMappings = pathMappings,
    }

    if also_packages then
      config.justMyCode = false
    end

    local session = dap.attach(adapter, config, {})
    if session == nil then
      io.write 'Error attaching adapter'
    end
  end

  local setup_vscode_js = function()
    -- one thing to note vscode-js-debug itself works as a server with an address and a port while it attaches to nodejs process via host:port
    -- and dap-vscode-js handles to run and connecting to vscode-js-debug server
    -- debugger_path is relying on ../debug.lua to install it
    require('dap-vscode-js').setup {
      -- node_path = "node", -- Path of node executable. Defaults to $NODE_PATH, and then "node"
      debugger_path = vim.fn.stdpath 'data' .. '/lazy/vscode-js-debug', -- Path to vscode-js-debug installation.
      -- debugger_cmd = { "js-debug-adapter" }, -- Command to use to launch the debug server. Takes precedence over `node_path` and `debugger_path`.
      adapters = { 'pwa-node', 'pwa-chrome', 'pwa-msedge', 'node-terminal', 'pwa-extensionHost' }, -- which adapters to register in nvim-dap
      -- log_file_path = "(stdpath cache)/dap_vscode_js.log" -- Path for file logging
      -- log_file_level = false -- Logging level for output to file. Set to false to disable file logging.
      -- log_console_level = vim.log.levels.ERROR -- Logging level for output to console. Set to false to disable console output.
    }

    -- add path for container if applicable, skip this if it's in the same host
    local remoteRoot = vim.env.DAP_NODEJS_REMOTE_ROOT or '${workspaceFolder}'
    local host = vim.env.DAP_NODEJS_REMOTE_HOST or 'localhost'
    local port = vim.env.DAP_NODEJS_REMOTE_PORT or '9229'

    -- options should match with the ones from:
    -- - https://github.com/microsoft/vscode-js-debug/blob/3477c7fe738e3ae5fc176f2e939c21d0cc703b8f/src/configuration.ts#L268
    -- - https://github.com/microsoft/vscode-js-debug/blob/3477c7fe738e3ae5fc176f2e939c21d0cc703b8f/src/configuration.ts#L512
    -- except possibly `type`, `name` that is for dap plugin
    local attachConfig = {
      type = 'pwa-node',
      request = 'attach',
      name = 'Attach',
      localRoot = '${workspaceFolder}',
      remoteRoot = remoteRoot,
      port = port,
      address = host,
    }

    for _, language in ipairs { 'typescript', 'javascript' } do
      require('dap').configurations[language] = {
        -- skipping setting a config for launch (file) as I'm currently not using that
        attachConfig,
      }
    end
  end

  local attach_keymap = function()
    local attach_to_debuggee = function()
      local ft = vim.bo.filetype
      local allowList = { javascript = true, typescript = true, lua = true }

      if ft == 'python' then
        attach_python()
      elseif allowList[ft] then
        -- currently only assume it's node
        local dap = require 'dap'
        -- dap.continue will trigger adapter and debugger to be active
        -- also relying on setup_vscode_js for adapters to be setup
        dap.continue()
      else
        print('currently there is no support for ' .. ft .. ' file type')
      end
    end

    vim.keymap.set('n', '<leader>ra', attach_to_debuggee, { noremap = true, desc = 'attach to debuggee' })
  end

  nlua()
  setup_dapui()
  setup_vscode_js()
  attach_keymap()
  require('nvim-dap-virtual-text').setup {}

  layer_keys()
end
