-- keymaps

--- my keymaps to to accomodate my muscle memory with ../SpaceVim.d
--- these may go to which-key side someday

-- put away `tags` according to https://github.com/ludovicchabant/vim-gutentags/issues/211
-- vim.g.gutentags_ctags_tagfile = '.git/gutentags'

-- because `use 'zhou13/vim-easyescape'` is too slow on startup
vim.api.nvim_set_keymap('i', 'jk', '<Esc>', { noremap = true, silent = true })
vim.api.nvim_set_keymap('n', '<Tab>', '<Cmd>wincmd w<CR>', { noremap = true, silent = true })

-- indent right away without waiting in normal mode
vim.api.nvim_set_keymap('n', '>', '>>', { noremap = true })
vim.api.nvim_set_keymap('n', '<', '<<', { noremap = true })

-- stay in visual mode after indentation in visual mode
vim.api.nvim_set_keymap('v', '>', '>gv', { noremap = true })
vim.api.nvim_set_keymap('v', '<', '<gv', { noremap = true })

-- q to close
vim.api.nvim_set_keymap('n', 'q', '<Cmd>q<CR>', { noremap = true })

--Remap space as leader key
vim.api.nvim_set_keymap('', '<Space>', '<Nop>', { noremap = true, silent = true })
-- vim.g.mapleader = ' '
-- vim.g.maplocalleader = ' '

--Remap for dealing with word wrap
vim.api.nvim_set_keymap('n', 'k', "v:count == 0 ? 'gk' : 'k'", { noremap = true, expr = true, silent = true })
vim.api.nvim_set_keymap('n', 'j', "v:count == 0 ? 'gj' : 'j'", { noremap = true, expr = true, silent = true })

--Add leader shortcuts
vim.api.nvim_set_keymap(
  'n',
  '<space><space>',
  [[<cmd>lua require('telescope.builtin').buffers()<CR>]],
  { noremap = true, silent = true }
)
vim.api.nvim_set_keymap(
  'n',
  '<space>sf',
  [[<cmd>lua require('telescope.builtin').find_files({previewer = false})<CR>]],
  { noremap = true, silent = true }
)
vim.api.nvim_set_keymap(
  'n',
  '<space>sb',
  [[<cmd>lua require('telescope.builtin').current_buffer_fuzzy_find()<CR>]],
  { noremap = true, silent = true }
)
vim.api.nvim_set_keymap(
  'n',
  '<space>sh',
  [[<cmd>lua require('telescope.builtin').help_tags()<CR>]],
  { noremap = true, silent = true }
)
vim.api.nvim_set_keymap(
  'n',
  '<space>st',
  [[<cmd>lua require('telescope.builtin').tags()<CR>]],
  { noremap = true, silent = true }
)
vim.api.nvim_set_keymap(
  'n',
  '<space>sd',
  [[<cmd>lua require('telescope.builtin').grep_string()<CR>]],
  { noremap = true, silent = true }
)
vim.api.nvim_set_keymap(
  'n',
  '<space>sp',
  [[<cmd>lua require('telescope.builtin').live_grep()<CR>]],
  { noremap = true, silent = true }
)
vim.api.nvim_set_keymap(
  'n',
  '<space>so',
  [[<cmd>lua require('telescope.builtin').tags{ only_current_buffer = true }<CR>]],
  { noremap = true, silent = true }
)
vim.api.nvim_set_keymap(
  'n',
  '<leader>?',
  [[<cmd>lua require('telescope.builtin').oldfiles()<CR>]],
  { noremap = true, silent = true }
)

-- Y yank until the end of line
vim.api.nvim_set_keymap('n', 'Y', 'y$', { noremap = true })

--- yup these above should go into which key one day definately!

-- use which-key to accomodate visual assistant on key-bindings ../SpaceVim.d

function Toggle_background()
  if vim.o.background == 'dark' then
    vim.o.background = 'light'
  else
    vim.o.background = 'dark'
  end
end

return {
  {
    'folke/which-key.nvim', -- show key bindings just like SpaceVim
    config = function()
      local paste_after_here = '"*p'
      local paste_before_here = '"*P'
      if vim.fn.has 'unnamedplus' == 1 then
        paste_after_here = '"+p'
        paste_before_here = '"+P'
      end

      require('which-key').register({
        p = { paste_after_here, 'paste after here' },
        P = { paste_before_here, 'paste before here' },
      }, { prefix = '<leader>' })

      require('which-key').register({
        [' '] = { '', 'remove trailing whitespaces' },
      }, { prefix = ',' })

      require('which-key').register({
        w = {
          m = { ':Goyo<CR>', 'maximize/minimize window' },
        },
        b = {
          b = { [[<cmd>lua require('telescope.builtin').buffers()<CR>]], 'search buffer' },
        },
        ['<Tab>'] = { ':bn<CR>', 'rotate buffer' },
        ["'"] = { ':ToggleTerm<CR>', 'open shell' },
        j = 'split args', -- only set a text for an already configured keymap
        ['<CR>'] = { '@q', 'macro q' }, -- setting a special key
        f = { -- set a nested structure
          name = '+find',
          b = { '<Cmd>Telescope buffers<CR>', 'buffers' },
          h = { '<Cmd>Telescope help_tags<CR>', 'help tags' },
          c = {
            name = '+commands',
            c = { '<Cmd>Telescope commands<CR>', 'commands' },
            h = { '<Cmd>Telescope command_history<CR>', 'history' },
          },
          q = { '<Cmd>Telescope quickfix<CR>', 'quickfix' },
          g = {
            name = '+git',
            g = { '<Cmd>Telescope git_commits<CR>', 'commits' },
            c = { '<Cmd>Telescope git_bcommits<CR>', 'bcommits' },
            b = { '<Cmd>Telescope git_branches<CR>', 'branches' },
            s = { '<Cmd>Telescope git_status<CR>', 'status' },
          },
          s = { ':w<CR>', 'save file' }, -- set a single command and text
          t = { ':NvimTreeToggle<CR>', 'toggle file tree' },
        },
        g = {
          b = { ':Git blame<CR>', 'toggle git blame' },
        },
        s = {
          name = '+Searching/Symbol',
          c = { '<Cmd>nohlsearch<CR>', 'clear hihglight' },
        },
        t = {
          name = '+UI Toggles',
          l = { '<Cmd>set list!<CR>', 'toggle-hidden-listchars' },
          b = { '<Cmd>lua Toggle_background()<CR>', 'toggle-background' },
        },
      }, { prefix = '<Space>' })
    end,
  },
}

-- vim: ts=2 sts=2 sw=2 et
