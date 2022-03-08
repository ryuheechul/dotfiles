-- originally from https://github.com/mjlbach/defaults.nvim/blob/73d4b205be5711b681ef2df9d171b1c55040803b/init.lua

-- workaround to be able to load other files while using alternative rtp
-- vim.cmd 'set rtp^=~/.config/my-quick-nvim' -- no longer necessary

-- via ./lua
require'plugins-via-packer'
require'misc'
require'lsp'
require'treesitter'
require'completion'
require'keymaps'
require'term'
-- end of via ./lua

-- vim: ts=2 sts=2 sw=2 et
