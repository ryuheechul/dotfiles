-- keymaps

--- my keymaps to to accomodate my muscle memory with ../SpaceVim.d
--- these may go to which-key side someday

-- put away `tags` according to https://github.com/ludovicchabant/vim-gutentags/issues/211
-- vim.g.gutentags_ctags_tagfile = '.git/gutentags'

-- because `use 'zhou13/vim-easyescape'` is too slow on startup
vim.keymap.set('i', 'jk', '<Esc>', { noremap = true, silent = true })
vim.keymap.set('n', '<Tab>', '<Cmd>wincmd w<CR>', { noremap = true, silent = true })

-- indent right away without waiting in normal mode
vim.keymap.set('n', '>', '>>', { noremap = true })
vim.keymap.set('n', '<', '<<', { noremap = true })

-- stay in visual mode after indentation in visual mode
vim.keymap.set('v', '>', '>gv', { noremap = true })
vim.keymap.set('v', '<', '<gv', { noremap = true })

-- q to close
-- use https://github.com/ojroques/nvim-bufdel
vim.keymap.set('n', 'q', '<Cmd>BufDel<CR>', { noremap = true })

-- -- Remap space as leader key - comment out since I'm not sure what this really does for me
-- vim.keymap.set('', '<Space>', '<Nop>', { noremap = true, silent = true })
-- vim.g.mapleader = ' '
-- vim.g.maplocalleader = ' '

-- Remap for dealing with word wrap
vim.keymap.set('n', 'k', "v:count == 0 ? 'gk' : 'k'", { noremap = true, expr = true, silent = true })
vim.keymap.set('n', 'j', "v:count == 0 ? 'gj' : 'j'", { noremap = true, expr = true, silent = true })

-- Y yank until the end of line
vim.keymap.set('n', 'Y', 'y$', { noremap = true })

-- , to repeat last normal command - https://stackoverflow.com/a/4789842/1570165
vim.keymap.set('n', ',', '@:<CR>', { noremap = true })

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

      local wk = require 'which-key'
      wk.setup {
        window = {
          winblend = 20,
        },
      }

      wk.register({
        p = { paste_after_here, 'paste after here' },
        P = { paste_before_here, 'paste before here' },
      }, { prefix = '<leader>' })

      -- this actually wasn't working - comment out until I revisit
      -- wk.register({
      --   [' '] = { '', 'remove trailing whitespaces' },
      -- }, { prefix = ',' })

      wk.register({
        w = {
          m = { ':WindowsMaximize<CR>', 'maximize/minimize window' },
          ['/'] = { ':vsplit<CR>', 'split window vertically' },
          ['-'] = { ':split<CR>', 'split window horizontally' },
        },
        b = {
          b = { [[<Cmd>lua require('telescope.builtin').buffers()<CR>]], 'search buffer' },
          d = { ':bd<CR>', 'close buffer' },
        },
        ['<space>'] = { '<Cmd>lua require("telescope.builtin").buffers()<CR>', 'telescope: buffers' },
        ['<Tab>'] = { ':bn<CR>', 'rotate buffer' },
        ["'"] = { ':FloatermToggle<CR>', 'open Floaterm' },
        ['/'] = { ':ToggleTerm<CR>', 'open ToggleTerm' },
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
          f = { '<Cmd>lua require("telescope.builtin").find_files({previewer = false})<CR>', 'find files' },
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
          d = { ':DiffviewOpen<CR>', 'show git diff' },
          n = { ':Neogit<CR>', 'open Neogit' },
        },
        p = {
          c = { ':PackerCompile<CR>', 'run :PackerCompile' },
          i = { ':PackerInstall<CR>', 'run :PackerInstall' },
          s = { ':PackerSync<CR>', 'run :PackerSync' },
          u = { ':PackerUpdate<CR>', 'run :PackerUpdate' },
        },
        s = {
          name = '+Searching/Symbol',
          ['?'] = { '<Cmd>lua require("telescope.builtin").oldfiles()<CR>', 'old files' },
          c = { '<Cmd>nohlsearch<CR>', 'clear hihglight' },
          f = { '<Cmd>lua require("telescope.builtin").find_files({previewer = false})<CR>', 'find files' },
          b = { '<Cmd>lua require("telescope.builtin").current_buffer_fuzzy_find()<CR>', 'current buffer fuzzy' },
          h = { '<Cmd>lua require("telescope.builtin").help_tags()<CR>', 'help tags' },
          t = { '<Cmd>lua require("telescope.builtin").tags()<CR>', 'tags' },
          d = { '<Cmd>lua require("telescope.builtin").grep_string()<CR>', 'grep string' },
          p = { '<Cmd>lua require("telescope.builtin").live_grep()<CR>', 'live grep' },
          o = {
            '<Cmd>lua require("telescope.builtin").tags{ only_current_buffer = true }<CR>',
            'tags only current buffer',
          },
          r = {
            '<Cmd>lua require"telescope".extensions.repo.list{search_dirs = {"~/play"}}<CR>',
            'Telescope repo',
          },
        },
        t = {
          name = '+UI Toggles',
          l = { '<Cmd>set list!<CR>', 'toggle-hidden-listchars' },
          b = { '<Cmd>lua Toggle_background()<CR>', 'toggle-background' },
        },
      }, { prefix = '<Space>' })
    end,
  },
  -- 'b0o/mapx.nvim', -- see if I would like to use this when keymapping code need optimized
}

-- vim: ts=2 sts=2 sw=2 et
