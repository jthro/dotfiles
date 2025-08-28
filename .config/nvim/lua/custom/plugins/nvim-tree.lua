return {
  "nvim-tree/nvim-tree.lua",
  version = "*",
  lazy = false,
  dependencies = {
    "nvim-tree/nvim-web-devicons",
  },
  config = function()
    local api = require("nvim-tree.api")

    require("nvim-tree").setup {
      on_attach = function (bufnr)
        local function opts (desc)
          return {
            desc = "nvim-tree: " .. desc,
            buffer = bufnr,
            noremap = true,
            silent = true,
            nowait = true,
          }
        end

        api.config.mappings.default_on_attach(bufnr)
        vim.keymap.set("n", "d", api.fs.remove, opts("Delete File"))
        vim.keymap.set("n", "o", api.fs.create, opts("Create File"))
        vim.keymap.set("n", "i", api.fs.rename, opts("Rename File"))
      end,

      view = {
        width = 40,
        side = "right",
      }
    }
  end,
}
