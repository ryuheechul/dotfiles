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

-- q to close in a smart way
vim.keymap.set('n', 'q', require 'utils.my-smart-quit', { noremap = true })

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

--- yup these above should go into which key one day definitely!

--- anything above here is not accessible from `config` function below
return {
  {
    'folke/which-key.nvim', -- show key bindings just like SpaceVim
    config = function()
      local cmdify = function(cmd)
        -- favor this since this is combinable
        return '<Cmd>' .. cmd .. '<CR>'

        -- -- it's possible to use the bottom version but this is not combinable
        -- return function()
        --   vim.cmd(cmd)
        -- end
      end

      local cmd_nohlsearch = cmdify 'nohlsearch'

      local toggle_bg = function()
        if vim.o.background == 'dark' then
          vim.o.background = 'light'
        else
          vim.o.background = 'dark'
        end
      end

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
          m = { cmdify 'WindowsMaximize', 'maximize/minimize window' },
          v = { cmdify 'WindowsMaximizeVertical', 'maximize/minimize window vertically' },
          h = { cmdify 'WindowsMaximizeHorizontal', 'maximize/minimize window horizontally' },
          ['/'] = { cmdify 'vsplit', 'split window vertically' },
          ['-'] = { cmdify 'split', 'split window horizontally' },
        },
        b = {
          b = { require('telescope.builtin').buffers, 'search buffer' },
          d = { cmdify 'bd', 'close buffer' },
        },
        ['<space>'] = { require('telescope.builtin').buffers, 'telescope: buffers' },
        ['<Tab>'] = { cmdify 'bn', 'rotate buffer' },
        ["'"] = { cmd_nohlsearch .. cmdify 'FloatermToggle', 'open Floaterm' },
        ['/'] = { cmd_nohlsearch .. cmdify 'ToggleTerm', 'open ToggleTerm' },
        j = 'split args', -- only set a text for an already configured keymap
        ['<CR>'] = { '@q', 'macro q' }, -- setting a special key
        f = { -- set a nested structure
          name = '+find',
          b = { cmdify 'Telescope buffers', 'buffers' },
          h = { cmdify 'Telescope help_tags', 'help tags' },
          c = {
            name = '+commands',
            c = { cmdify 'Telescope commands', 'commands' },
            h = { cmdify 'Telescope command_history', 'history' },
          },
          f = {
            function()
              require('telescope.builtin').find_files { previewer = false }
            end,
            'find files',
          },
          q = { cmdify 'Telescope quickfix', 'quickfix' },
          g = {
            name = '+git',
            g = { cmdify 'Telescope git_commits', 'commits' },
            c = { cmdify 'Telescope git_bcommits', 'bcommits' },
            b = { cmdify 'Telescope git_branches', 'branches' },
            s = { cmdify 'Telescope git_status', 'status' },
          },
          n = { cmdify 'new', 'new file' },
          r = { require('gfold').pick_repo, 'pick repo' },
          s = { cmdify 'w', 'save file' },
          t = { cmdify 'NvimTreeToggle', 'toggle file tree' },
        },
        g = {
          b = { cmdify 'Git blame', 'toggle git blame' },
          d = { cmdify 'DiffviewOpen', 'show git diff' },
          n = { cmdify 'Neogit', 'open Neogit' },
        },
        p = {
          -- not only compile also generates helptags proactively in case of some missing helptags
          c = { cmdify 'PackerCompile' .. cmdify 'helptags ALL', 'run :PackerCompile' },
          i = { cmdify 'PackerInstall', 'run :PackerInstall' },
          s = { cmdify 'PackerSync', 'run :PackerSync' },
          u = { cmdify 'PackerUpdate', 'run :PackerUpdate' },
        },
        s = {
          name = '+Searching/Symbol',
          ['?'] = { require('telescope.builtin').oldfiles, 'old files' },
          c = { cmd_nohlsearch, 'clear hihglight' },
          f = {
            function()
              require('telescope.builtin').find_files { previewer = false }
            end,
            'find files',
          },
          b = { require('telescope.builtin').current_buffer_fuzzy_find, 'current buffer fuzzy' },
          h = { require('telescope.builtin').help_tags, 'help tags' },
          t = { require('telescope.builtin').tags, 'tags' },
          d = { require('telescope.builtin').grep_string, 'grep string' },
          p = { require('telescope.builtin').live_grep, 'live grep' },
          o = {
            function()
              require('telescope.builtin').tags { only_current_buffer = true }
            end,
            'tags only current buffer',
          },
          r = {
            function()
              require('telescope').extensions.repo.list { search_dirs = { '~/play' } }
            end,
            'Telescope repo',
          },
        },
        t = {
          name = '+UI Toggles',
          l = { cmdify 'set list!', 'toggle-hidden-listchars' },
          b = { toggle_bg, 'toggle-background' },
        },
      }, { prefix = '<Space>' })
    end,
  },
  -- 'b0o/mapx.nvim', -- see if I would like to use this when keymapping code need optimized
}

-- vim: ts=2 sts=2 sw=2 et
