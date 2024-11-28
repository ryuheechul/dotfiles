-- term settings

return function()
  --- augment terminal with env vars
  -- cleaner approach than $FORCE_LOAD_MY_ZSH_STUFF
  vim.env.UNSET_MY_BASIC_ZSH_STUFF_LOADED = 1
  vim.env.UNSET_ALL_MY_ZSH_STUFF_LOADED = 1
  vim.env.UNSET_HOST_ALWAYS_USE_TMUX = 1
  vim.env.NO_VI_KEY_ON_ZSH = 1

  -- e.g. calc_length('columns'|'lines', 0.[1-9])
  local calc_length = function(opt, ratio)
    return function()
      return math.floor(vim.o[opt] * ratio)
    end
  end

  -- based on https://github.com/akinsho/toggleterm.nvim/blob/b02a1674bd0010d7982b056fd3df4f717ff8a57a/lua/toggleterm.lua#L77-L81
  local function focus_nth_term(num)
    local terms = require 'toggleterm.terminal'
    local lazy = require 'toggleterm.lazy'
    local ui = lazy.require 'toggleterm.ui'
    local term = terms.get_or_create_term(num)
    ui.update_origin_window(term.window)

    -- avoid opening another one when it's already open
    if not term:is_open() then
      term:open()
    end
  end

  require('toggleterm').setup {
    -- size can be a number or function which is passed the current terminal
    size = function(term)
      if term.direction == 'horizontal' then
        return 15
      elseif term.direction == 'vertical' then
        return vim.o.columns * 0.4
      end
    end, -- | 20
    open_mapping = [[<c-\>]],
    hide_numbers = true, -- hide the number column in toggleterm buffers
    shade_filetypes = {},
    shade_terminals = false,
    shading_factor = 0, -- the degree by which to darken to terminal colour, default: 1 for dark backgrounds, 3 for light
    start_in_insert = true,
    insert_mappings = true, -- whether or not the open mapping applies in insert mode
    persist_size = true,
    direction = 'horizontal', -- | 'vertical' | 'window' | 'float',
    close_on_exit = true, -- close the terminal window when the process exits
    shell = vim.o.shell, -- change the default shell
    -- This field is only relevant if direction is set to 'float'
    float_opts = {
      -- The border key is *almost* the same as 'nvim_win_open'
      -- see :h nvim_win_open for details on borders however
      -- the 'curved' border is a custom border type
      -- not natively supported but implemented in this plugin.
      border = 'rounded', -- | 'single' | 'double' | 'shadow' | 'curved' | ... other options supported by win open
      width = calc_length('columns', 0.6),
      height = calc_length('lines', 0.6),
      winblend = 20,
      highlights = {
        border = 'Normal',
        background = 'Normal',
      },
    },
  }

  local cmdify = function(cmd)
    return '<Cmd>' .. cmd .. '<CR>'
  end

  function _G.set_terminal_keymaps()
    local opts = { buffer = 0 }
    vim.keymap.set('t', '<esc>', [[<C-\><C-n>]], opts)
    -- -- disable this as it makes long press with `j` not rendering until sometime after the key release.
    -- -- see https://github.com/akinsho/toggleterm.nvim/issues/63 for more details
    -- vim.keymap.set('t', 'jk', [[<C-\><C-n>]], opts)
    vim.keymap.set('n', '<esc>', cmdify 'close', opts)
    vim.keymap.set('t', '<C-h>', cmdify 'NavigatorLeft', opts)
    vim.keymap.set('t', '<C-j>', cmdify 'NavigatorDown', opts)
    vim.keymap.set('t', '<C-k>', cmdify 'NavigatorUp', opts)
    vim.keymap.set('t', '<C-l>', cmdify 'NavigatorRight', opts)
  end

  local termGrp = vim.api.nvim_create_augroup('MyTerm', { clear = true })
  -- used to be `vim.cmd 'autocmd! TermOpen term://* lua set_terminal_keymaps()'`
  vim.api.nvim_create_autocmd('TermOpen', {
    -- if you only want these mappings for toggle term use term://*toggleterm#* instead
    pattern = 'term://*',
    callback = set_terminal_keymaps,
    group = termGrp,
  })

  -- setup a handler to bring the most recent terminal back
  vim.api.nvim_create_autocmd('TermEnter', {
    pattern = 'term://*',
    callback = function()
      local bufnr = vim.api.nvim_get_current_buf()
      local bufname = vim.api.nvim_buf_get_name(bufnr)

      local termnr_str = string.match(bufname, 'toggleterm#(%d+)$')
      local termnr = tonumber(termnr_str)

      -- this global function gets updated every time so whoever calls `:lua FocusRecentToggleTerm()` can bring the most recent terminal back to focus
      function _G.FocusRecentToggleTerm()
        focus_nth_term(termnr)
      end
    end,
    group = termGrp,
  })
end
