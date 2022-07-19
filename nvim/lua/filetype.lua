-- where I customize editing related behaviours per file type

vim.cmd [[
autocmd FileType markdown setlocal shiftwidth=2 softtabstop=2 expandtab
autocmd FileType typescript setlocal shiftwidth=2 softtabstop=2 expandtab
autocmd FileType Earthfile setlocal shiftwidth=2 softtabstop=2 expandtab
autocmd FileType json setlocal shiftwidth=2 softtabstop=2 expandtab
]]

-- vim: ts=2 sts=2 sw=2 et
