-- use which-key to accomodate visual assistant on key-bindings ../SpaceVim.d
paste_after_here = '"*p'
paste_before_here = '"*P'
if vim.fn.has('unnamedplus') == 1 then
  paste_after_here = '"+p'
  paste_before_here = '"+P'
end

require('which-key').register({
    p = {paste_after_here, 'paste after here'},
    P = {paste_before_here, 'paste before here'},
}, { prefix = '<leader>' })

require('which-key').register({
    [' '] = {'', 'remove trailing whitespaces'},
}, { prefix = ',' })


function toggle_background()
  if vim.o.background == 'dark' then
    vim.o.background = 'light'
  else
    vim.o.background = 'dark'
  end
end

require('which-key').register({
    w = {
      m = {':Goyo<CR>', 'maximize/minimize window'}
    },
    b = {
      b = {[[<cmd>lua require('telescope.builtin').buffers()<CR>]], 'search buffer'}
    },
    ['<Tab>'] = {':bn<CR>', 'rotate buffer'},
    ["'"] = {':ToggleTerm<CR>', 'open shell'},
    j = 'split args', -- only set a text for an already configured keymap
    ['<CR>'] = {'@q', 'macro q'}, -- setting a special key
    f = { -- set a nested structure
        name = '+find',
        b = {'<Cmd>Telescope buffers<CR>', 'buffers'},
        h = {'<Cmd>Telescope help_tags<CR>', 'help tags'},
        c = {
            name = '+commands',
            c = {'<Cmd>Telescope commands<CR>', 'commands'},
            h = {'<Cmd>Telescope command_history<CR>', 'history'},
        },
        q = {'<Cmd>Telescope quickfix<CR>', 'quickfix'},
        g = {
            name = '+git',
            g = {'<Cmd>Telescope git_commits<CR>', 'commits'},
            c = {'<Cmd>Telescope git_bcommits<CR>', 'bcommits'},
            b = {'<Cmd>Telescope git_branches<CR>', 'branches'},
            s = {'<Cmd>Telescope git_status<CR>', 'status'},
        },
        s = {':w<CR>', 'save file'}, -- set a single command and text
        t = {':NvimTreeToggle<CR>', 'toggle file tree'},
    },
    g = {
        b = {':VGit toggle_live_blame<CR>', 'toggle git blame in live'},
    },
    s = {
        name = '+Searching/Symbol',
        c = {'<Cmd>nohlsearch<CR>', 'clear hihglight'},
    },
    t = {
        name = '+UI Toggles',
        l = {'<Cmd>set list!<CR>', 'toggle-hidden-listchars'},
        b = {'<Cmd>lua toggle_background()<CR>', 'toggle-background'},
    },

}, { prefix = '<Space>' })

-- put away `tags` according to https://github.com/ludovicchabant/vim-gutentags/issues/211
-- vim.g.gutentags_ctags_tagfile = '.git/gutentags'

--- my keymaps to to accomodate my muscle memory with ../SpaceVim.d

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
