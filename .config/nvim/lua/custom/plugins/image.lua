return {
  opts = {
    rocks = { 'magick' },
  },
  "3rd/image.nvim",
  config = function()
    require("image").setup({
      integrations = {
        markdown = {
          enabled = true,
        },
      },
    })
  end
}

