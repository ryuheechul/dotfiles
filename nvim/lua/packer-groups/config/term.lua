-- term settings

return function()
  -- augment terminal
  vim.cmd [[
        " cleaner approach than $FORCE_LOAD_MY_ZSH_STUFF
        let $UNSET_MY_BASIC_ZSH_STUFF_LOADED=1
        let $UNSET_ALL_MY_ZSH_STUFF_LOADED=1

        let $NO_VI_KEY_ON_ZSH = 1
      ]]

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
    shade_terminals = true,
    shading_factor = 3, -- the degree by which to darken to terminal colour, default: 1 for dark backgrounds, 3 for light
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
      border = 'single', -- | 'double' | 'shadow' | 'curved' | ... other options supported by win open
      -- width = <value>,
      -- height = <value>,
      winblend = 3,
      highlights = {
        border = 'Normal',
        background = 'Normal',
      },
    },
  }

  function _G.set_terminal_keymaps()
    local opts = { buffer = 0 }
    vim.keymap.set('t', '<esc>', [[<C-\><C-n>]], opts)
    -- -- disable this as it makes long press with `j` not rendering until sometime after the key release.
    -- -- see https://github.com/akinsho/toggleterm.nvim/issues/63 for more details
    -- vim.keymap.set('t', 'jk', [[<C-\><C-n>]], opts)
    vim.keymap.set('t', '<C-h>', [[<Cmd>wincmd h<CR>]], opts)
    vim.keymap.set('t', '<C-j>', [[<Cmd>wincmd j<CR>]], opts)
    vim.keymap.set('t', '<C-k>', [[<Cmd>wincmd k<CR>]], opts)
    vim.keymap.set('t', '<C-l>', [[<Cmd>wincmd l<CR>]], opts)
  end

  -- if you only want these mappings for toggle term use term://*toggleterm#* instead
  vim.cmd 'autocmd! TermOpen term://* lua set_terminal_keymaps()'
end
