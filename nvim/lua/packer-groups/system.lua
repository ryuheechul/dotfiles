-- system level plugins or regarding integration

return {
  'christoomey/vim-system-copy', -- copy text to clipboard with `cp`
  'roxma/vim-tmux-clipboard', -- share clipboard with tmux
  'christoomey/vim-tmux-navigator', -- navigate with tmux key binding
  'akinsho/nvim-toggleterm.lua', -- a great ergonomic terminal customization
  -- UI to select things (files, grep results, open buffers...)
{ 'nvim-telescope/telescope.nvim',
    requires = { 'nvim-lua/popup.nvim', 'nvim-lua/plenary.nvim' },
    config = function ()
      require'telescope'.setup {
        defaults = {
          mappings = {
            i = {
              ['<C-u>'] = false,
              ['<C-d>'] = false,
            },
          },
        },
      }
    end
  },
{ 'kyazdani42/nvim-tree.lua',
    requires = 'kyazdani42/nvim-web-devicons',
    config = function()
      -- `nvim_tree_callback` and `cb` is deprecated
      local tree_cb = require'nvim-tree.config'.nvim_tree_callback

      require'nvim-tree'.setup {
        view = {
          side = 'right',
          mappings = {
            custom_only = false,
            -- override default mappings
            list = {
            { key = {"o", "<2-LeftMouse>", "l"},         action = "edit" },
              -- { key = {"<2-RightMouse>", "<C-]>", "<CR>"}, cb = tree_cb("cd") },
            { key = {"-", "h"},                          action = "dir_up" },
            { key = {"<Tab>"},                           cb = ':wincmd w<CR>' },
            }
          },
        },
        -- auto_open = true,
        auto_close = false,
        disable_netrw = false,
        hijack_netrw = false,
        open_on_setup = true,
      }
    end
  }, -- enhanced filetree replacing netrw
{ 'subnut/nvim-ghost.nvim',
    run = ':call nvim_ghost#installer#install()' }, -- https://github.com/fregante/GhostText
}

-- vim: ts=2 sts=2 sw=2 et
