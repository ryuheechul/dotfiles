local wezterm = require 'wezterm'
local config = {}
if wezterm.config_builder then
  config = wezterm.config_builder()
end

-- Font
config.font = wezterm.font 'FiraMono Nerd Font Mono'
config.font_size = 15.0

-- UI
config.hide_tab_bar_if_only_one_tab = true
config.window_decorations = 'RESIZE'

-- Env
config.set_environment_variables = {
  UNSET_HOST_ALWAYS_USE_TMUX = '1',
}

return config
