-- luasnip settings

return function()
  -- Set completeopt to have a better completion experience
  vim.o.completeopt = 'menuone,noselect'

  -- luasnip setup - https://github.com/nvim-lua/kickstart.nvim/blob/9288f4adcb25ebc70dab2ba0cd6b910b1fde57bf/init.lua#L270

  local luasnip = require 'luasnip'
  local lspkind = require 'lspkind'

  local has_words_before = function()
    local line, col = unpack(vim.api.nvim_win_get_cursor(0))
    return col ~= 0 and vim.api.nvim_buf_get_lines(0, line - 1, line, true)[1]:sub(col, col):match '%s' == nil
  end

  local source_mapping = {
    buffer = '[buf]',
    nvim_lsp = '[LSP]',
    nvim_lua = '[api]',
    path = '[path]',
    luasnip = '[snip]',
    gh_issues = '[issues]',
    cmp_tabnine = '[TabNine]',
  }

  -- nvim-cmp setup
  local cmp = require 'cmp'
  cmp.setup {
    snippet = {
      expand = function(args)
        luasnip.lsp_expand(args.body)
      end,
    },
    mapping = {
      ['<C-p>'] = cmp.mapping.select_prev_item(),
      ['<C-n>'] = cmp.mapping.select_next_item(),
      ['<C-d>'] = cmp.mapping.scroll_docs(-4),
      ['<C-f>'] = cmp.mapping.scroll_docs(4),
      ['<C-Space>'] = cmp.mapping.complete(),
      ['<C-e>'] = cmp.mapping {
        i = cmp.mapping.abort(),
        c = cmp.mapping.close(),
      },
      ['<CR>'] = cmp.mapping.confirm {
        behavior = cmp.ConfirmBehavior.Replace,
        select = true,
      },
      ['<c-y>'] = cmp.mapping(
        cmp.mapping.confirm {
          behavior = cmp.ConfirmBehavior.Insert,
          select = true,
        },
        { 'i', 'c' }
      ),
      ['<Tab>'] = cmp.mapping(function(fallback)
        if cmp.visible() then
          cmp.select_next_item()
        elseif luasnip.expand_or_jumpable() then
          luasnip.expand_or_jump()
        elseif has_words_before() then
          cmp.complete()
        else
          fallback()
        end
      end, { 'i', 's' }),
      ['<S-Tab>'] = cmp.mapping(function(fallback)
        if cmp.visible() then
          cmp.select_prev_item()
        elseif luasnip.jumpable(-1) then
          luasnip.jump(-1)
        else
          fallback()
        end
      end, { 'i', 's' }),
    },
    sources = {
      { name = 'gh_issues' },
      -- Could enable this only for lua, but nvim_lua handles that already.
      { name = 'nvim_lua' },
      { name = 'nvim_lsp' },
      { name = 'path' },
      -- { name = "cmdline" }, -- I don't understand what this is yet so skip loading for now.
      { name = 'luasnip' },
      { name = 'buffer', keyword_length = 4 },
      { name = 'cmp_tabnine', keyword_length = 4 },
    },
    formatting = {
      -- -- this is simpler but less customization
      -- format = lspkind.cmp_format {
      --   with_text = true,
      --   menu = source_mapping
      -- },
      -- this is on top of above but extra style for TabNine
      format = function(entry, vim_item)
        vim_item.kind = lspkind.presets.default[vim_item.kind]
        local menu = source_mapping[entry.source.name]
        if entry.source.name == 'cmp_tabnine' then
          if entry.completion_item.data ~= nil and entry.completion_item.data.detail ~= nil then
            menu = entry.completion_item.data.detail .. ' ' .. menu
          end
          vim_item.kind = ''
        end
        vim_item.menu = menu
        return vim_item
      end,
    },
    experimental = {
      ghost_text = true, -- this feature conflict to the copilot.vim's preview.
    },
  }

  -- load including'rafamadriz/friendly-snippets'
  require('luasnip.loaders.from_vscode').load()

  -- require("luasnip.loaders.from_vscode").load({ paths = { "./snippets" } }) -- Load snippets from my-snippets folder
  -- not sure how to do relative path properly like above, so just doing the below for now instead
  require('luasnip.loaders.from_vscode').load { paths = { '~/dotfiles/nvim/snippets' } } -- Load snippets from my-snippets folder

  -- Every unspecified option will be set to the default.
  luasnip.config.set_config {
    history = true,
    -- Update more often, :h events for more info.
    updateevents = 'TextChanged,TextChangedI',
  }
end

-- vim: ts=2 sts=2 sw=2 et
