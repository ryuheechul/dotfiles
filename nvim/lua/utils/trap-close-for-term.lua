-- call it via `:lua require('utils.trap-close-for-term')()`
-- to accomodate toggleterm to use host neovim to edit files
return function()
  local ttGrp = vim.api.nvim_create_augroup('MyTrapCloseTT', { clear = true })
  local bufnr = vim.fn.bufnr '%'

  -- this is to react with `:[w]q`
  vim.api.nvim_create_autocmd({ 'QuitPre' }, {
    buffer = 0,
    callback = function()
      -- this is to trigger autocmd to bring toggleterm back
      vim.cmd [[doautocmd BufUnload]]
      -- this needs to be called before actually deleting the buffer
      -- to notify waiting is done at https://github.com/mhinz/neovim-remote/blob/1004d41696a3de12f0911b1949327c3dbe4a62ab/nvr/nvr.py#L114
      vim.cmd [[doautocmd BufDelete]]
      -- and actual deletion of the buffer
      vim.cmd('bd ' .. bufnr)
    end,
    group = ttGrp,
  })

  -- since we are relying on event with `q`, we set the event here
  vim.api.nvim_create_autocmd({ 'BufUnload' }, {
    buffer = 0,
    callback = function()
      vim.defer_fn(function()
        -- to deal with the created tab not get cleaned up with `q` (but `:q` cleans it up)
        local tabs = vim.api.nvim_list_tabpages()
        if #tabs > 1 then
          vim.cmd [[ tabclose ]]
        end

        -- TODO: for now assume that it's coming from floating one but this should be addressed
        vim.cmd [[ 9ToggleTerm ]]
      end, 1)
    end,
    group = ttGrp,
  })
end
