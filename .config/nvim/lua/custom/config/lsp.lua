local lspconfig = require("lspconfig")
local blink_cmp = require("blink.cmp")

local servers = {
  rust_analyzer = {
    settings = {
      ["rust-analyzer"] = {
        check = {
          command = "clippy",
          extraArgs = {
            "--",
            "-Wclippy::pedantic",
          },
        },
        checkOnSave = true,
      },
    },
  },

  lua_ls = {},
  ts_ls = {},
  cssls = {},
  emmet_ls = {},
  pyright = {},
  verible = {},
}

for server, config in pairs(servers) do
  config.capabilities = blink_cmp.get_lsp_capabilities(config.capabilities)
  lspconfig[server].setup(config)
end

-- Add verilog support
vim.api.nvim_create_autocmd('FileType', {
  pattern = {'verilog', 'systemverilog'},
  callback = function()
    vim.lsp.start({
      name = 'verible',
      cmd = {'verible-verilog-ls', '--rules_config_search'},
    })
  end,
})

vim.api.nvim_create_autocmd("BufWritePost", {
  pattern = "*.v",
  callback = function ()
    vim.lsp.buf.format({ async = false })
  end
})

vim.api.nvim_create_autocmd({"BufNewFile", "BufRead"}, {
  pattern = {"*.v"},
  command = "set filetype=verilog",
})

-- Diagnostic
vim.diagnostic.config({
  signs = true,
  underline = true,
  update_in_insert = false,
  virtual_text = {
    prefix = "▎", -- Could be "●", "▎", "x", "■"
  },
})
