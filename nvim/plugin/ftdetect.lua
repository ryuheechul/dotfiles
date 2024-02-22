-- go to `../lua/boot/filetype.lua` for filetype specific configurations

vim.cmd [[
au BufRead,BufNewFile Earthfile set filetype=Earthfile
au BufRead,BufNewFile *.kdl set filetype=kdl
au BufRead,BufNewFile *.astro set filetype=astro
]]

-- resorting to above while below is not working for some reason

-- vim.filetype.add {
--   extension = {
--     kdl = 'kdl',
--     astro = 'astro',
--   },
--   filename = {
--     Earthfile = 'Earthfile',
--   },
-- }
