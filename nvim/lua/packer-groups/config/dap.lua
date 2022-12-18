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
    vim.keymap.set('n', '<space>rl', function()
      require('osv').launch { port = 8086 }
    end, { noremap = true, desc = 'DAP OSV launch' })

    -- for debugger - Neovim instance B that will debug instance A
    vim.keymap.set('n', '<space>rc', dap.continue, { noremap = true, desc = 'DAP continue' })
    vim.keymap.set('n', '<space>rr', dap.repl.toggle, { noremap = true, desc = 'DAP repl toggle' })
    vim.keymap.set('n', '<space>rb', dap.toggle_breakpoint, { noremap = true, desc = 'DAP toggle breakpoint' })
    vim.keymap.set('n', '<space>rso', dap.step_over, { noremap = true, desc = 'DAP step over' })
    vim.keymap.set('n', '<space>rsi', dap.step_into, { noremap = true, desc = 'DAP step into' })
    vim.keymap.set('n', '<space>rh', require('dap.ui.widgets').hover, { noremap = true, desc = 'DAP hover' })
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

  nlua()
end
