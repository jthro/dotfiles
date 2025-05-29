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
  css_ls = {},
  emmet_ls = {},
}

for server, config in pairs(servers) do
  config.capabilities = blink_cmp.get_lsp_capabilities(config.capabilities)
  lspconfig[server].setup(config)
end

-- Diagnostic
vim.diagnostic.config({
  signs = true,
  underline = true,
  update_in_insert = false,
  virtual_text = {
    prefix = "▎", -- Could be "●", "▎", "x", "■"
  },
})
