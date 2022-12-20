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
    vim.keymap.set('n', '<leader>rc', dap.continue, { noremap = true, desc = 'DAP continue' })
    -- vim.keymap.set('n', '<leader>rr', dap.repl.toggle, { noremap = true, desc = 'DAP repl toggle' })
    vim.keymap.set('n', '<leader>rb', dap.toggle_breakpoint, { noremap = true, desc = 'DAP toggle breakpoint' })
    vim.keymap.set('n', '<leader>rso', dap.step_over, { noremap = true, desc = 'DAP step over' })
    vim.keymap.set('n', '<leader>rsi', dap.step_into, { noremap = true, desc = 'DAP step into' })
    vim.keymap.set('n', '<leader>rh', require('dap.ui.widgets').hover, { noremap = true, desc = 'DAP hover' })
    -- .exit               Closes the REPL
    -- .c or .continue     Same as |dap.continue|
    -- .n or .next         Same as |dap.step_over|
    -- .into               Same as |dap.step_into|
    -- .into_target        Same as |dap.step_into{askForTargets=true}|
    -- .out                Same as |dap.step_out|
    -- .up                 Same as |dap.up|
    -- .down               Same as |dap.down|
    -- .goto               Same as |dap.goto_|
    -- .scopes             Prints the variables in the current scopes
    -- .threads            Prints all threads
    -- .frames             Print the stack frames
    -- .capabilities       Print the capabilities of the debug adapter
    -- .b or .back         Same as |dap.step_back|
    -- .rc or
    -- .reverse-continue   Same as |dap.reverse_continue|
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

  local attach_python = function()
    local dap = require 'dap'
    local host = vim.env.DAP_PYTHON_REMOTE_HOST or 'localhost'
    local port = vim.env.DAP_PYTHON_REMOTE_PORT or '5678'
    local portnr = tonumber(port)

    -- Wherever your Python code lives locally.
    local localRoot = vim.fn.getcwd()
    -- Wherever your Python code lives in the container.
    local remoteRoot = vim.env.DAP_PYTHON_REMOTE_ROOT or '/app'

    local pythonAttachConfig = {
      type = 'python',
      request = 'attach',
      connect = {
        port = portnr,
        host = host,
      },
      mode = 'remote',
      name = 'Remote Attached Debugger',
      cwd = vim.fn.getcwd(),
      pathMappings = {
        {
          localRoot = localRoot,
          remoteRoot = remoteRoot,
        },
      },
    }
    local session = dap.attach(host, portnr, pythonAttachConfig)
    if session == nil then
      io.write 'Error launching adapter'
    end
  end

  vim.keymap.set('n', '<leader>rd', attach_python, { noremap = true, desc = 'attach to python' })
  nlua()
  dapui()
end
