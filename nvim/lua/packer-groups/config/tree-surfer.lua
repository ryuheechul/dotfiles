-- syntax-tree-surfer configuration

-- tl;dr:
--
-- | mode | keys    | description                       |
-- |----- | ------- | --------------------------------- |
-- | n    | vU      | swap master node with above       |
-- |      | vD      | swap master node with below       |
-- |  n   | ------- | --------------------------------- |
-- |  o   | vu      | swap current node with above      |
-- |  r   | vd      | swap current node with below      |
-- |  m   | ------- | --------------------------------- |
-- |  a   | vx      | select master node                |
-- |  l   | vn      | select current node               |
-- |----- | ------- | --------------------------------- |
-- | x    | n       | select next sibling               |
-- |      | N       | select previous sibling           |
-- |  v   | ------- | --------------------------------- |
-- |  i   | ]]      | select parent                     |
-- |  s   | [[      | select child                      |
-- |  u   | ------- | --------------------------------- |
-- |  a   | m       | swap with next sibling            |
-- |  l   | M       | swap with previous sibling        |

return function()
  require('syntax-tree-surfer').setup {}

  -- Normal Mode Swapping:
  -- Swap The Master Node relative to the cursor with it's siblings, Dot Repeatable
  vim.keymap.set('n', 'vU', function()
    vim.opt.opfunc = 'v:lua.STSSwapUpNormal_Dot'
    return 'g@l'
  end, { silent = true, expr = true, desc = 'Swap master node with above' })
  vim.keymap.set('n', 'vD', function()
    vim.opt.opfunc = 'v:lua.STSSwapDownNormal_Dot'
    return 'g@l'
  end, { silent = true, expr = true, desc = 'Swap master node with below' })

  -- Swap Current Node at the Cursor with it's siblings, Dot Repeatable
  vim.keymap.set('n', 'vu', function()
    vim.opt.opfunc = 'v:lua.STSSwapCurrentNodePrevNormal_Dot'
    return 'g@l'
  end, { silent = true, expr = true, desc = 'Swap current node with above' })
  vim.keymap.set('n', 'vd', function()
    vim.opt.opfunc = 'v:lua.STSSwapCurrentNodeNextNormal_Dot'
    return 'g@l'
  end, { silent = true, expr = true, desc = 'Swap current node with below' })

  local opts = { noremap = true, silent = true }

  local desc_opts = function(desc)
    return require('utils.table').merge(opts, { desc = desc })
  end

  -- Visual Selection from Normal Mode
  vim.keymap.set('n', 'vx', '<cmd>STSSelectMasterNode<cr>', desc_opts 'select master node')
  vim.keymap.set('n', 'vn', '<cmd>STSSelectCurrentNode<cr>', desc_opts 'select current node')

  -- Select Nodes in Visual Mode
  for _, key in ipairs {
    'n',
    -- 'j', -- `hjkl` aliases are convenient but it messes with basic movement so better not set
    -- 'l',
  } do
    vim.keymap.set('x', key, '<cmd>STSSelectNextSiblingNode<cr>', desc_opts 'select next sibling')
  end

  for _, key in ipairs {
    'N',
    -- 'k',
    -- 'h',
  } do
    vim.keymap.set('x', key, '<cmd>STSSelectPrevSiblingNode<cr>', desc_opts 'select previous sibling')
  end

  -- expand/narrow node selection in Visual Mode
  vim.keymap.set('x', '[[', '<cmd>STSSelectParentNode<cr>', desc_opts 'select parent')
  vim.keymap.set('x', ']]', '<cmd>STSSelectChildNode<cr>', desc_opts 'select child')

  -- Swapping Nodes in Visual Mode
  vim.keymap.set('x', 'm', '<cmd>STSSwapNextVisual<cr>', desc_opts 'swap with next sibling')
  vim.keymap.set('x', 'M', '<cmd>STSSwapPrevVisual<cr>', desc_opts 'swap with previous sibling')
end

-- vim: ts=2 sts=2 sw=2 et
