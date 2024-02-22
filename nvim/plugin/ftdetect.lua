-- go to `../lua/boot/filetype.lua` for filetype specific configurations

-- sometimes highlighting via treesitter doesn't work until `:TSEnable highlight`
-- - which is strange
-- - debug whether the filetype is set properly or not, regardless of highlighting via `:set ft?` or `echo &filetype`

vim.cmd [[
au BufRead,BufNewFile Earthfile set filetype=Earthfile
au BufRead,BufNewFile *.kdl set filetype=kdl
au BufRead,BufNewFile *.astro set filetype=astro
au BufRead,BufNewFile *.hurl set filetype=hurl
]]

-- resorting to above while below is not working for some reason

-- vim.filetype.add {
--   extension = {
--     kdl = 'kdl',
--     hurl = 'hurl',
--     astro = 'astro',
--   },
--   filename = {
--     Earthfile = 'Earthfile',
--   },
-- }
