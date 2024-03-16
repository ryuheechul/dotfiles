-- go to `../lua/boot/filetype.lua` for filetype specific configurations

vim.filetype.add {
  extension = {
    kdl = 'kdl',
    hurl = 'hurl',
    astro = 'astro',
  },
  filename = {
    Earthfile = 'Earthfile',
    ['.envrc'] = 'bash',
  },
}

-- above replaces the old way like below
--
-- vim.cmd [[
-- au BufRead,BufNewFile Earthfile set filetype=Earthfile
-- au BufRead,BufNewFile *.kdl set filetype=kdl
-- au BufRead,BufNewFile *.astro set filetype=astro
-- au BufRead,BufNewFile *.hurl set filetype=hurl
-- ]]
