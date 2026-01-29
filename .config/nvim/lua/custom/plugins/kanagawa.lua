return {
  "rebelot/kanagawa.nvim",
  lazy = false,
  priority = 1000,
  opts = {},
  config = function ()
    require("kanagawa").load("dragon")
  end
}
