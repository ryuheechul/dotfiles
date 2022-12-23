-- to bridge between keymap-layer and winbar on lualine
-- usage: `{ require('utils.keymap-layer').get_summary, cond = require('utils.keymap-layer').determine_active }`
return {
  determine_active = function()
    local activelayer = _G.active_keymap_layer
    return activelayer and activelayer.active
  end,
  get_summary = function()
    local activelayer = _G.active_keymap_layer
    if (not activelayer) or not activelayer.active then
      return 'no active layer found'
    end

    local kms = activelayer.layer_keymaps

    -- only care about normal mode keys for now
    local normal_kms = kms['n']

    local summaries = {}

    -- collect description of each key
    for key, value in pairs(normal_kms) do
      local opt = value[2]
      local desc = '[' .. key .. ']' .. opt['desc']
      table.insert(summaries, desc)
    end

    local summary = table.concat(summaries, ' ')
    return 'LAYER: ' .. summary
  end,
}
