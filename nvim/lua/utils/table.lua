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
