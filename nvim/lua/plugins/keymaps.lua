-- keymaps

local cmdify = function(cmd)
  -- favor this since this is combinable
  return '<Cmd>' .. cmd .. '<CR>'

  -- -- it's possible to use the bottom version but this is not combinable
  -- return function()
  --   vim.cmd(cmd)
  -- end
end

--- key mapping regardless of `which-key` for "essential" stuff
local init = function()
  vim.keymap.set('n', 'g:', 'gQ', { noremap = true, silent = false, desc = 'enter Ex-mode' })
  vim.keymap.set('n', 'g;', 'q:', { noremap = true, silent = true, desc = 'open command-line window' })
  -- there is also `g;` `g,` `gi`
  -- - https://learnbyexample.github.io/tips/vim-tip-5/
  -- - https://www.reddit.com/r/vim/comments/nrkvwm/just_discovered_ctrlo_and_ctrli_and_it_rules/
  vim.keymap.set('n', 'go', '<C-O>', { noremap = true, silent = true, desc = 'Go to "old"' })
  -- FYI, <C-I> and <Tab> are same in vim
  -- - https://stackoverflow.com/a/18176199/1570165
  vim.keymap.set('n', 'gn', '<C-I>', { noremap = true, silent = true, desc = 'Go to "new"' })
  vim.keymap.set('n', 'gl', '<C-]>', { noremap = true, silent = true, desc = 'go to the link under the cursor' })

  -- because `zhou13/vim-easyescape` is too slow on startup
  vim.keymap.set('i', 'jk', '<Esc>', { noremap = true, silent = true, desc = 'shortcut to <Esc>' })
  vim.keymap.set('n', '<Tab>', '<Cmd>wincmd w<CR>', { noremap = true, silent = true, desc = 'go to "next" window' })
  vim.keymap.set(
    'n',
    '<S-Tab>',
    '<Cmd>wincmd W<CR>',
    { noremap = true, silent = true, desc = 'go to "previous" window' }
  )
  vim.keymap.set('n', 't', cmdify 'Telescope buffers', { noremap = true, silent = true, desc = ':Telescope buffers' })

  -- replace builtin spell suggestions - see `:h z=`
  vim.keymap.set('n', 'z=', cmdify 'Telescope spell_suggest', { noremap = true, desc = 'fix spelling' })

  -- -- Remap space as leader key - comment out since I'm not sure what this really does for me
  -- vim.keymap.set('', '<Space>', '<Nop>', { noremap = true, silent = true })
  -- vim.g.mapleader = ' '
  -- vim.g.maplocalleader = ' '

  -- Remap for dealing with word wrap
  vim.keymap.set('n', 'k', "v:count == 0 ? 'gk' : 'k'", {
    noremap = true,
    expr = true,
    silent = true,
    desc = 'to work better with word wrap',
  })
  vim.keymap.set('n', 'j', "v:count == 0 ? 'gj' : 'j'", {
    noremap = true,
    expr = true,
    silent = true,
    desc = 'to work better with word wrap',
  })

  -- Y yank until the end of line
  vim.keymap.set('n', 'Y', 'y$', { noremap = true, desc = 'yank until the end of line' })

  -- , to repeat last normal command - https://stackoverflow.com/a/4789842/1570165
  vim.keymap.set('n', ',', '@:<CR>', { noremap = true, desc = 'repeat last normal command' })

  -- since I don't use the default way of `+`, `-`, mapping these to increase/decrease numbers
  -- - https://learnbyexample.github.io/tips/vim-tip-1/
  vim.keymap.set('n', '+', '<C-A>', { noremap = true, silent = true, desc = 'increase number' })
  vim.keymap.set('n', '-', '<C-X>', { noremap = true, silent = true, desc = 'decrease number' })
end

local toggle_bg = function()
  if vim.o.background == 'dark' then
    vim.o.background = 'light'
  else
    vim.o.background = 'dark'
  end
end

local config = function()
  local cmd_nohlsearch = cmdify 'nohlsearch'

  local paste_after_here = '"*p'
  local paste_before_here = '"*P'
  if vim.fn.has 'unnamedplus' == 1 then
    paste_after_here = '"+p'
    paste_before_here = '"+P'
  end

  local wk = require 'which-key'
  wk.setup {
    -- ignore warning on start up but can manually check via `:checkhealth which-key`
    notify = false,
  }

  wk.add {
    { '<leader>p', paste_after_here, desc = 'paste after here' },
    { '<leader>P', paste_before_here, desc = 'paste before here' },
    { '<leader>e', group = 'Eval' },
    { '<leader>el', cmdify 'FennelRepl', desc = 'Fennel Repl' },
    { '<leader>es', cmdify 'SnipRun', desc = 'SnipRun' },
    { '<leader>r', group = 'Run/Debug' },
    { '<leader>rc', group = 'Conjure' },
    { '<leader>g', group = 'Generate' },
    { '<leader>l', cmdify 'Luapad', desc = 'open Luapad' },
    { '<leader>n', group = 'Neotest' },
    { '<leader>nr', cmdify 'Neotest run', desc = 'Neotest run' },
    { '<leader>t', group = 'Treesitter' },
    { '<leader>tt', cmdify 'TSPlaygroundToggle', desc = 'TSPlaygroundToggle' },
    { '<leader>s', cmdify 'suspend', desc = ':suspend - go to background - `fg` to comeback' },
  }

  wk.add {
    {
      "<Space>'",
      -- use count 9 to be independent from the horizontal one
      cmd_nohlsearch .. cmdify '9ToggleTerm direction=float',
      desc = 'open ToggleTerm direction=float',
    },
    {
      '<Space>/',
      -- use count 8 to be independent from the float one
      cmd_nohlsearch .. cmdify '8ToggleTerm direction=horizontal',
      desc = 'open ToggleTerm direction=horizontal',
    },
    { '<Space><CR>', '@q', desc = 'macro q' }, -- setting a special key
    { '<Space><Tab>', cmdify 'bn', desc = 'rotate buffer' },
    { '<Space><space>', cmdify 'Telescope oldfiles', desc = 'Telescope: recent files' },
  }

  wk.add {
    { '<Space>b', group = 'Buffers' },
    { '<Space>bb', cmdify 'Telescope buffers', desc = 'search buffer' },
    { '<Space>bd', cmdify 'bd', desc = 'close buffer' },
  }

  wk.add {
    { '<Space>f', group = 'Find' },
    { '<Space>fb', cmdify 'Telescope buffers', desc = 'buffers' },
    { '<Space>fc', group = 'Commands' },
    { '<Space>fcc', cmdify 'Telescope commands', desc = 'commands' },
    { '<Space>fch', cmdify 'Telescope command_history', desc = 'history' },
    {
      '<Space>fd',
      function()
        require('telescope').extensions.dap.commands()
      end,
      desc = 'dap commands',
    },
    {
      '<Space>fe',
      function()
        vim.cmd([[silent exec "!open 'org-protocol://find-file?path=]] .. vim.fn.expand '%:p' .. [['"]])
      end,
      desc = 'open in Emacs',
    },
    { '<Space>ff', cmdify 'Telescope find_files', desc = 'find files' },
    { '<Space>fg', group = 'Git' },
    { '<Space>fgb', cmdify 'Telescope git_branches', desc = 'branches' },
    { '<Space>fgc', cmdify 'Telescope git_bcommits', desc = 'bcommits' },
    { '<Space>fgg', cmdify 'Telescope git_commits', desc = 'commits' },
    { '<Space>fgs', cmdify 'Telescope git_status', desc = 'status' },
    { '<Space>fh', cmdify 'Telescope help_tags', desc = 'help tags' },
    { '<Space>fk', cmdify 'Telescope keymaps', desc = 'search keymaps' },
    { '<Space>fn', cmdify 'new', desc = 'new file' },
    { '<Space>fq', cmdify 'Telescope quickfix', desc = 'quickfix' },
    { '<Space>fr', cmdify 'Telescope oldfiles', desc = 'recent files' },
    { '<Space>fs', cmdify 'w', desc = 'save file' },
    { '<Space>ft', cmdify 'Telescope', desc = 'telescope' },
  }

  wk.add {
    { '<Space>g', group = 'Git' },
    { '<Space>gb', group = 'Blame' },
    { '<Space>gbb', cmdify 'Git blame', desc = 'toggle git blame pane' },
    { '<Space>gd', cmdify 'DiffviewOpen', desc = 'show git diff' },
    -- { '<Space>gg', require('gfold').pick_repo, desc = 'pick repo via gfold' },
    { '<Space>gn', cmdify 'Neogit', desc = 'open Neogit' },
    { '<Space>gr', cmdify 'Gcd', desc = 'go to git root' },
    { '<Space>gx', cmdify 'GBrowse', desc = 'open file in browser' },
  }

  wk.add {
    { '<Space>j', cmdify 'normal Hzb', desc = 'previous page' },
    { '<Space>k', cmdify 'normal Lzt', desc = 'next page' },
  }

  wk.add {
    { '<Space>l', group = 'Lazy' },
    { '<Space>ll', cmdify 'Lazy', desc = 'run :Lazy' },
    { '<Space>ls', cmdify 'Lazy sync', desc = 'run :Lazy sync' },
    { '<Space>lu', cmdify 'Lazy update', desc = 'run :Lazy update' },
    { '<Space>li', cmdify 'Lazy install', desc = 'run :Lazy install' },
    { '<Space>lp', cmdify 'Lazy profile', desc = 'run :Lazy profile' },
  }

  wk.add {
    { '<Space>r', group = 'Rename' },
  }

  wk.add {
    { '<Space>s', group = 'Searching/Symbol' },
    { '<Space>s?', cmdify 'Telescope oldfiles', desc = 'old files' },
    { '<Space>sb', cmdify 'Telescope current_buffer_fuzzy_find', desc = 'current buffer fuzzy' },
    { '<Space>sc', cmdify 'nohlsearch', desc = 'clear hihglight' },
    { '<Space>sd', cmdify 'Telescope grep_string', desc = 'grep string' },
    { '<Space>sf', cmdify 'Telescope find_files', desc = 'find files' },
    {
      '<Space>sg',
      function()
        require('telescope').extensions.repo.list { search_dirs = { '~/play' } }
      end,
      desc = 'Telescope repo',
    },
    { '<Space>sh', cmdify 'Telescope help_tags', desc = 'help tags' },
    {
      '<Space>so',
      function()
        require('telescope.builtin').tags { only_current_buffer = true }
      end,
      desc = 'tags only current buffer',
    },
    { '<Space>sp', cmdify 'Telescope live_grep', desc = 'live grep in project' },
    { '<Space>st', cmdify 'Telescope tags', desc = 'tags' },
  }

  wk.add {
    { '<Space>t', group = 'UI Toggles' },
    { '<Space>tb', toggle_bg, desc = 'toggle-background' },
    -- Lf is better for preview purposes and more freedom but will fail on remote files
    { '<Space>tf', cmdify 'Lf', desc = 'toggle lf' },
    { '<Space>tl', cmdify 'set list!', desc = 'toggle-hidden-listchars' },
    -- Oil is preferred on efficient bulk renaming of filenames or
    {
      '<Space>to',
      function()
        require('oil').toggle_float()
      end,
      desc = 'toggle Oil',
    },
  }

  wk.add {
    { '<Space>w', group = 'Windows/Workspace' },
    { '<Space>w-', cmdify 'split', desc = 'split window horizontally' },
    { '<Space>w/', cmdify 'vsplit', desc = 'split window vertically' },
    { '<Space>wd', cmdify 'close', desc = 'close window' },
    { '<Space>wh', cmdify 'WindowsMaximizeHorizontally', desc = 'maximize/minimize window horizontally' },
    { '<Space>wv', cmdify 'WindowsMaximizeVertically', desc = 'maximize/minimize window vertically' },
    {
      '<Space>wm',
      cmdify 'silent exec "!mux-zoom"' .. cmdify 'WindowsMaximize',
      desc = 'maximize/minimize window',
    },
    {
      '<Space>wn',
      cmdify(vim.env.ZELLIJ_SESSION_NAME and '!zellij action new-tab' or '!tmux new-window'),
      desc = 'new (t)mux window',
    },
    {
      '<Space>wp',
      cmdify(vim.env.ZELLIJ_SESSION_NAME and '!zellij action new-pane' or '!tmux split-window'),
      desc = 'new (t)mux panel',
    },
    -- few convenient tmux controls - see also ../../../zsh/my_addons/aliases
    -- no more ctrl+a[key] when not necessary!
    {
      '<Space>wt',
      cmdify(
        'silent exec '
          .. '"!'
          .. (vim.env.ZELLIJ_SESSION_NAME and 'zellij action launch-plugin -f sessionpicker' or 'tmux choose-tree')
          .. '"'
      ),
      desc = 'choose (t)mux tree',
    },
  }
end

--- anything above here is not accessible from `config` function below
return {
  {
    'folke/which-key.nvim', -- show key bindings just like SpaceVim
    init = init,
    event = 'VeryLazy',
    config = config,
    dependencies = {
      'echasnovski/mini.icons', -- Icon provider. Part of 'mini.nvim' library.
    },
  },
  -- 'b0o/mapx.nvim', -- see if I would like to use this when keymapping code need optimized
}

-- vim: ts=2 sts=2 sw=2 et
