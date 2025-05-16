return {
  "nvim-treesitter/nvim-treesitter",
  version = "*",
  build = ":TSUpdate",
  lazy = false,
  config = function()
    require("custom.config.treesitter").setup()
  end,
}
