-- this is to mimic the search and yank experience of https://github.com/schasse/tmux-jump
-- in fact the experience of this seems mostly better for my use case
-- it is loaded via `../init.lua`

local searchGrp = vim.api.nvim_create_augroup('MyZellijSearchAUG', { clear = true })

-- use 'VimEnter' instead of 'BufEnter' to prevent influencing a normal buffer:
-- e.g. moving to previous cursor opens a buffer of any `.dump` buffer;
--      which causes a potential closing due to the 'TextYankPost' on a normal buffer
vim.api.nvim_create_autocmd('VimEnter', {
  pattern = '*.dump',
  callback = function()
    -- set autocmd to quit as soon as TextYankPost event happens
    vim.api.nvim_create_autocmd('TextYankPost', {
      callback = function()
        vim.defer_fn(function()
          require 'utils.my-smart-quit'()
        end, 1)
      end,
      group = searchGrp,
    })

    vim.defer_fn(function()
      -- https://www.reddit.com/r/neovim/comments/104lc26/how_can_i_press_escape_key_using_lua/
      vim.api.nvim_feedkeys('/', 'n', false)
    end, 100)
  end,
  group = searchGrp,
})
