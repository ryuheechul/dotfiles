return {
  -- https://stackoverflow.com/a/71433446/1570165
  merge = function(a, b)
    local c = {}
    for k, v in pairs(a) do
      c[k] = v
    end
    for k, v in pairs(b) do
      c[k] = v
    end
    return c
  end,
}

-- -- usage example
-- local merge = require('utils.table').merge
--
-- local t1 = { a = 0, b = 2 }
-- local t2 = { a = 1, c = 3 }
--
-- print(
-- merge(t1,t2)
-- )
--
-- -- this will not work very well with dict-array-mixed-tables
--
-- -- in those cases just inserting at the end would be an workaround
-- -- assuming `b` is fully array table
-- for _, v in ipairs(b) do
--   table.insert(c, v)
-- end
