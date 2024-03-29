-- system level plugins or regarding integration

return {
  -- two plugins below are probably no longer necessary due to `vim.opt.clipboard = 'unnamedplus'`
  -- 'christoomey/vim-system-copy', -- copy text to clipboard with `cp`
  -- 'roxma/vim-tmux-clipboard', -- share clipboard with tmux
  { -- A Neovim plugin to improve buffer deletion
    'ojroques/nvim-bufdel',
    dependencies = {
      -- https://github.com/akinsho/bufferline.nvim/issues/239#issuecomment-944153281
      'famiu/bufdelete.nvim', -- Delete Neovim buffers without losing window layout
    },
    event = 'VimEnter',
    config = function()
      -- q to close in a smart way
      vim.keymap.set('n', 'q', require 'utils.my-smart-quit', { noremap = true, desc = 'quit smarter' })
    end,
  },
  { 'christoomey/vim-tmux-navigator', event = 'VimEnter' }, -- navigate with tmux key binding
  { -- a great ergonomic terminal customization
    'akinsho/toggleterm.nvim',
    config = require 'plugins.config.term',
    event = 'VeryLazy',
  },
  --- although floaterm was cool to use and convenient, I met a weird dead-end performance issue on typing only with the combination of followings
  --- bufferline.nvim (only when tab is visible) + pyright lsp (with many files unlisted) + vim-floaterm activated (even after closed the term)
  --- therefore I just use toggleterm instead to mimic the usage of floaterm
  --- indirectly related issue - https://github.com/neovim/neovim/issues/21452
  -- 'voldikss/vim-floaterm', -- 🌟 Terminal manager for (neo)vim
  { -- UI to select things (files, grep results, open buffers...)
    'nvim-telescope/telescope.nvim',
    event = 'VeryLazy',
    dependencies = {
      -- beginning of core dependencies
      'nvim-lua/plenary.nvim',
      'nvim-lua/popup.nvim',
      -- end of core dependencies
      'crispgm/telescope-heading.nvim', -- An extension for telescope.nvim that allows you to switch between headings
      {
        'ANGkeith/telescope-terraform-doc.nvim',
        -- temp workaround to deal with broken head
        commit = '73291b564fed413ced13e890144d359793b3860c',
      },
      'camgraff/telescope-tmux.nvim',
      'benfowler/telescope-luasnip.nvim',
      'xiyaowong/telescope-emoji.nvim',
      'cljoly/telescope-repo.nvim',
      'nvim-telescope/telescope-dap.nvim',
    },
    config = function()
      local telescope = require 'telescope'
      local actions = require 'telescope.actions'

      telescope.setup {
        defaults = {
          winblend = 20,
          mappings = {
            i = {
              ['<C-u>'] = false,
              ['<C-d>'] = false,
              ['<Tab>'] = actions.move_selection_next,
              ['<S-Tab>'] = actions.move_selection_previous,
            },
          },
        },
        pickers = {
          find_files = {
            theme = 'ivy',
          },
          spell_suggest = {
            theme = 'cursor',
          },
        },
      }

      local exts = { 'heading', 'terraform_doc', 'tmux', 'luasnip', 'emoji', 'repo', 'dap' }

      for _, ext in ipairs(exts) do
        telescope.load_extension(ext)
      end
    end,
  },
  { -- Lf file manager for Neovim (in Lua)
    'lmburns/lf.nvim',
    opts = {
      border = 'rounded',
      winblend = 20,
      highlights = {
        NormalFloat = {
          guibg = '', -- "normalize"
        },
        FloatBorder = {
          guifg = '#aaaaaa',
        },
      },
    },
    dependencies = { 'plenary.nvim', 'toggleterm.nvim' },
  },
  { -- edit your filesystem like a normal Neovim buffer
    -- handy for quickly editing remote files - but becareful as it will overwrite the file on the remote host and vice versa if simulatanously being edited
    -- e.g. `:edit [user@]oil-ssh://remote-host//home/user/directory/`
    -- or `:edit oil-ssh://remote-host/directory/` for the same as above
    -- but not `:edit oil-ssh://remote-host:~/directory/` nor `:edit oil-ssh://remote-host/~/directory/`
    -- as it's not the same as using `scp` command in a shell which typically i would use like `scp remote-host:~/directory/file .`
    -- editing remote file is also possible even without oil though - https://gist.github.com/RRethy/ad8a9a3b1112a48226ec3336fa981224
    'stevearc/oil.nvim',
    dependencies = { 'nvim-tree/nvim-web-devicons' },
    event = 'VeryLazy',
    opts = {
      view_options = {
        show_hidden = true,
      },
      keymaps = {
        ['<Esc>'] = 'actions.close',
        ['e'] = 'actions.select',
        -- ['l'] = 'actions.select', this would be convenient and not at the same time
        -- - convenient for entering sub directory the same way as lf
        -- - inconvenient for editing the buffer freely
        -- `-`: navigate to parent directory
        -- see more default keymaps via `:h oil-config`
      },
      float = {
        max_width = 100,
        max_height = 30,
        win_options = {
          winblend = 20,
        },
      },
    },
  },
  { -- A file explorer tree for neovim written in lua
    'nvim-tree/nvim-tree.lua',
    event = 'VeryLazy',
    dependencies = 'nvim-tree/nvim-web-devicons',
    opts = {
      view = {
        side = 'right',
      },
      -- auto_open = true,
      -- not available any more
      -- https://github.com/kyazdani42/nvim-tree.lua/blob/b2ba6dea7105d9afabd3af08abd93947b851a90f/lua/nvim-tree/legacy.lua#L213-L218
      -- auto_close = false,
      disable_netrw = false,
      hijack_netrw = false,
      on_attach = function(bufnr)
        local api = require 'nvim-tree.api'

        local function opts(desc)
          return { desc = 'nvim-tree: ' .. desc, buffer = bufnr, noremap = true, silent = true, nowait = true }
        end

        -- use default mappings
        api.config.mappings.default_on_attach(bufnr)

        -- open file in buffer
        vim.tbl_map(function(key)
          vim.keymap.set('n', key, api.node.open.edit, { buffer = bufnr })
        end, { 'o', '<2-LeftMouse>', 'l', 'e' })

        -- as well as `-`
        vim.keymap.set('n', 'h', api.tree.change_root_to_parent, opts 'Up')
        -- to be able to "toggle" the behavior of `change_root_to_parent`
        vim.keymap.set('n', '=', api.tree.change_root_to_node, opts 'Change Root')
        -- cycle windows
        vim.keymap.set('n', '<Tab>', '<Cmd>wincmd w<CR>', { buffer = bufnr })
      end,
    },
  }, -- enhanced filetree replacing netrw
  { -- https://github.com/fregante/GhostText
    'subnut/nvim-ghost.nvim',
    build = ':call nvim_ghost#installer#install()',
    cond = function()
      return vim.env.my_nvim_ghost ~= nil
    end,
  },
  { -- A snazzy bufferline for Neovim - an upgrade from 'ap/vim-buftabline'
    'akinsho/bufferline.nvim',
    event = 'VeryLazy',
    version = 'v3.*',
    dependencies = 'nvim-tree/nvim-web-devicons',
    opts = {
      options = {
        diagnostics = 'nvim_lsp',
        always_show_bufferline = false,
      },
    },
  },
  { -- Simple winbar/statusline plugin that shows your current code context
    'SmiteshP/nvim-navic',
    event = 'VeryLazy',
    opts = {
      lazy_update_context = true,
      click = true,
    },
  },
  { -- A VS Code like winbar for Neovim
    'utilyre/barbecue.nvim',
    dependencies = {
      'neovim/nvim-lspconfig',
      'SmiteshP/nvim-navic',
      'nvim-tree/nvim-web-devicons',
    },
    event = 'VeryLazy',
    cond = function()
      return vim.env.my_nvim_winbar_barbecue ~= nil
    end,
    config = true,
  },
  { -- A blazing fast and easy to configure Neovim statusline written in Lua
    'nvim-lualine/lualine.nvim',
    dependencies = {
      'nvim-tree/nvim-web-devicons',
      'SmiteshP/nvim-navic',
    },
    event = 'FocusGained',
    config = require 'plugins.config.bars',
  },
  { -- A lua profiler for neovim that is discovered thanks to https://www.reddit.com/r/neovim/comments/xicxox/comment/ip2hprd
    'stevearc/profile.nvim',
    config = require 'plugins.config.profile',
    cond = function()
      return vim.env.NVIM_PROFILE ~= nil
    end,
  },
}

-- vim: ts=2 sts=2 sw=2 et
