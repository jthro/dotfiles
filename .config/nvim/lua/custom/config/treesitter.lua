M = {}

M.setup = function()
  local configs = require("nvim-treesitter.configs")
  configs.setup({
    ensure_installed = { "c", "lua", "vim", "vimdoc", "javascript", "html" },
    sync_install = false,
    highlight = { enable = true },
    indent = { enable = true },
  })
end

return M
