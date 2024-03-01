-- THIS IS NOW DEPRECATED, AS I REVERTED THE DECISION ON USING THE SHIM
-- FROM THIS COMMIT https://github.com/ryuheechul/dotfiles/commit/0a8bae199ea8151e1b90b4075e8925a6717839f5

-- to help dealing with nixos quarks

-- also see these files:
-- - ../utils/nixos-shim.lua
-- - ../../../nix/home/shims.nix
local merge = require('utils.table').merge

local should_override_for_nixos = vim.env.my_system_nixos ~= nil

local nvim_treesitter_base = {
  'nvim-treesitter/nvim-treesitter',
  -- thanks to https://www.reddit.com/r/NixOS/comments/17el4x7/comment/k63xbwi
  dev = should_override_for_nixos,
}

return {
  nvim_treesitter = {
    -- currently I use `.base` to mark the dependency
    -- however, somehow that prevents the main one, `.extend` to not call the `config` function for some reason
    -- - I suspect that it's probably something to do with `dev` being turned on
    -- hence not giving the real thing to compromise:
    -- - plugins still get to mark the dependency
    -- - by giving empty plugin, the main one still get to work "properly"
    -- - and this workaround should fix the slowness that I documented at [../plugins/README.md](../plugins/README.md#performance)
    base = should_override_for_nixos and {} or nvim_treesitter_base,
    -- base = nvim_treesitter_base,
    extend = function(tbl)
      return merge(nvim_treesitter_base, tbl)
    end,
  },
}
