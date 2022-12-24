return {
  -- -- usage example
  -- local merge = require('utils.table').merge
  --
  -- local t1 = { a = 0, b = 2 }
  -- local t2 = { a = 1, c = 3 }
  --
  -- print(
  --   merge(t1,t2) -- basically same as vim.tbl_extend("force",a,b)
  -- )
  --
  -- -- this will not work very well with dict-array-mixed-tables
  --
  -- -- in those cases just inserting at the end would be an workaround
  -- -- assuming `b` is fully array table
  -- for _, v in ipairs(b) do
  --   table.insert(c, v)
  -- end
  merge = function(a, b)
    -- https://stackoverflow.com/a/71433446/1570165
    local c = {}
    for k, v in pairs(a) do
      c[k] = v
    end
    for k, v in pairs(b) do
      c[k] = v
    end
    return c
  end,
  -- -- usage example
  -- local keys = require('utils.table').keys
  -- keys({ key1 = 1, key2 = 2 }) -- should result to be something like `{ 'key1', 'key2' }`
  keys = function(tbl)
    -- https://stackoverflow.com/a/12674376/1570165
    local keyset = {}
    local n = 0

    for k, _ in pairs(tbl) do
      n = n + 1
      keyset[n] = k
    end
    return keyset
  end,
}
