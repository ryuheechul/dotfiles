-- go to `../lua/boot/filetype.lua` for filetype specific configurations

vim.filetype.add {
  extension = {
    kdl = 'kdl',
    hurl = 'hurl',
    astro = 'astro',
    alloy = 'alloy',
  },
  filename = {
    Earthfile = 'Earthfile',
    ['.envrc'] = 'bash',
  },
  -- Detect and apply filetypes based on certain patterns of the filenames
  pattern = {
    -- INFO: Match filenames like - ".env.example", ".env.local" and so on
    ['%.env%.[%w_.-]+'] = 'dotenv',
    -- https://neovim.discourse.group/t/how-to-add-custom-filetype-detection-to-various-env-files/4272/3
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
