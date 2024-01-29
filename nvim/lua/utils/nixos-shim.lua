-- to help dealing with nixos quarks

-- also see these files:
-- - ../utils/nixos-shim.lua
-- - ../../../nix/home/shims.nix
local merge = require('utils.table').merge

local nvim_treesitter_base = {
  'nvim-treesitter/nvim-treesitter',
  -- thanks to https://www.reddit.com/r/NixOS/comments/17el4x7/comment/k63xbwi
  dev = vim.env.my_system_nixos ~= nil,
}

return {
  nvim_treesitter = {
    base = nvim_treesitter_base,
    extend = function(tbl)
      return merge(nvim_treesitter_base, tbl)
    end,
  },
}
