-- workaround to be able to load other files while using alternative rtp
-- vim.cmd 'set rtp^=~/.config/my-quick-nvim' -- no longer necessary

-- to improve startup time
-- this doesn't fail when `impatient` doesn't exist yet while `require 'impatient'` fails
local ok, impaitent = pcall(require, 'impatient')
if ok and vim.env.NVIM_CACHE_PROFILE ~= nil then
  impaitent.enable_profile()
end

-- via ./lua
require 'plugins-via-packer'
require 'filetype'
require 'misc'
-- end of via ./lua

-- vim: ts=2 sts=2 sw=2 et
