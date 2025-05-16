return {
  "nvim-neorg/neorg",
  build = ":Neorg sync-parsers",
  opts = {
    load = {
      ["core.defaults"] = {},
      ["core.concealer"] = {},
      ["core.dirman"] = {
        config = {
          workspaces = {
            notes = "~/notes",
          },
          default_workspaces = "notes",
        },
      },
      ["core.export.markdown"] = {
        config = {
          extensions = { "mathematics" },
        },
      },
      ["core.export"] = {},
      ["core.integrations.treesitter"] = {},
      ["core.latex.renderer"] = {
        config = {
          conceal = true,
          render_on_enter = true,
        },
      },
    },
  },
  dependencies = {
    { "nvim-lua/plenary.nvim", "3rd/image.nvim" },
  },
  lazy = false,
  version = "*",
  config = true,
}
