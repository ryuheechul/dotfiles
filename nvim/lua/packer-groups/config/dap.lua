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
    vim.keymap.set('n', '<leader>rh', require('dap.ui.widgets').hover, { noremap = true, desc = 'DAP hover' })

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
        { m, 't', dap.toggle_breakpoint, { noremap = true, desc = 'DAP toggle breakpoints' } },
        { m, 'i', dap.step_into, { noremap = true, desc = 'DAP step into' } },
        { m, 'o', dap.step_out, { noremap = true, desc = 'DAP step out' } },
        { m, 'n', dap.step_over, { noremap = true, desc = 'DAP step over' } },
        { m, 'p', dap.step_back, { noremap = true, desc = 'DAP step back' } },
        { m, 'c', dap.continue, { noremap = true, desc = 'DAP continue' } },
        { m, 'q', dap.continue, { noremap = true, desc = 'DAP continue' } },
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
      io.write 'Error launching adapter'
    end
  end

  vim.keymap.set('n', '<leader>ra', attach_python, { noremap = true, desc = 'attach to python' })
  nlua()
  dapui()
  require('nvim-dap-virtual-text').setup()

  layer_keys()
end
