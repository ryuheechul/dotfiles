-- DAP settings

return function()
  -- wonder why this function doesn't work when it's at the top level of this file when ../../utils/my-smart-quit.lua seems to work just fine
  -- probably because how packer works
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
      callback { type = 'server', host = config.host or '127.0.0.1', port = config.port or 8086 }
    end

    -- for debuggee - Neovim instance A that wants to be debugged
    -- so launch this at the neovim client that is a debuggee which also runs the server that debugger client can attach
    vim.keymap.set('n', '<leader>ro', function()
      require('osv').launch { port = 8086 }
    end, { noremap = true, desc = 'DAP OSV launch' })

    -- for debugger - Neovim instance B that will debug instance A
    -- rely most of dap commands via Telescope instead of keybindings - currently it's mapped to <space>fd
    -- also with quick_debug keymap layer
  end

  local dapui = function()
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
            'console',
          },
          size = 10, -- columns
          position = 'bottom',
        },
      },
      controls = {
        -- enabled = vim.fn.exists '+winbar' == 1,
        enabled = 1,
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

    dap.listeners.after.event_initialized['dapui_config'] = function()
      dapui.open()
    end
    dap.listeners.before.event_terminated['dapui_config'] = function()
      dapui.close()
    end
    dap.listeners.before.event_exited['dapui_config'] = function()
      dapui.close()
    end

    vim.keymap.set('n', '<leader>ru', dapui.toggle, { noremap = true, desc = 'DAP UI toggle' })
  end

  local layer_keys = function()
    local dap = require 'dap'
    local KeyLayer = require 'keymap-layer'

    local m = { 'n' } -- modes
    local quick_debug = KeyLayer {
      enter = {},
      layer = {
        { m, 'p', require('dap.ui.widgets').hover, { noremap = true, desc = 'hover' } },
        { m, 't', dap.toggle_breakpoint, { noremap = true, desc = 'tgl breakpoints' } },
        { m, 'i', dap.step_into, { noremap = true, desc = 'step into' } },
        { m, 'o', dap.step_out, { noremap = true, desc = 'step out' } },
        { m, 'n', dap.step_over, { noremap = true, desc = 'step over' } },
        { m, 'p', dap.step_back, { noremap = true, desc = 'step back' } },
        { m, 'c', dap.continue, { noremap = true, desc = 'continue' } },
        { m, 'q', dap.continue, { noremap = true, desc = 'continue' } },
      },
      exit = {},
      config = {
        on_enter = function()
          print 'Enter quick debug layer'
          -- vim.bo.modifiable = false
        end,
        on_exit = function()
          print 'Exit quick debug layer'
        end,
      },
    }
    -- my_dap_kb: my dap keybindings
    dap.listeners.after.event_stopped['my_dap_kb'] = function(--[[ session, body ]])
      quick_debug:activate()
    end
    dap.listeners.after.event_continued['my_dap_kb'] = function(--[[ session, body ]])
      quick_debug:exit()
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

    local session = dap.attach(adapter, config)
    if session == nil then
      io.write 'Error attaching adapter'
    end
  end

  local setup_vscode_js = function()
    -- debugger_path is relying on ../debug.lua to install it
    require('dap-vscode-js').setup {
      -- node_path = "node", -- Path of node executable. Defaults to $NODE_PATH, and then "node"
      -- debugger_path = "(runtimedir)/site/pack/packer/opt/vscode-js-debug", -- Path to vscode-js-debug installation.
      -- debugger_cmd = { "js-debug-adapter" }, -- Command to use to launch the debug server. Takes precedence over `node_path` and `debugger_path`.
      adapters = { 'pwa-node', 'pwa-chrome', 'pwa-msedge', 'node-terminal', 'pwa-extensionHost' }, -- which adapters to register in nvim-dap
      -- log_file_path = "(stdpath cache)/dap_vscode_js.log" -- Path for file logging
      -- log_file_level = false -- Logging level for output to file. Set to false to disable file logging.
      -- log_console_level = vim.log.levels.ERROR -- Logging level for output to console. Set to false to disable console output.
    }

    -- add path for container if applicable, skip this if it's in the same host
    local remoteRoot = vim.env.DAP_NODEJS_REMOTE_ROOT or '${workspaceFolder}'

    -- currently this works as expected for docker containers that expose port to 9229
    -- this is a bit different from an example from https://github.com/mxsdev/nvim-dap-vscode-js
    -- I frankly have no idea why this works clearly yet but hooray it works!
    local attachConfig = {
      type = 'pwa-node',
      request = 'attach',
      name = 'Attach',
      localRoot = '${workspaceFolder}',
      remoteRoot = remoteRoot,
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
        print('current there is no support for ' .. ft .. ' file type')
      end
    end

    vim.keymap.set('n', '<leader>ra', attach_to_debuggee, { noremap = true, desc = 'attach to debuggee' })
  end

  nlua()
  dapui()
  setup_vscode_js()
  attach_keymap()
  require('nvim-dap-virtual-text').setup()

  layer_keys()
end
