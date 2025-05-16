return {
  "startup-nvim/startup.nvim",
  dependencies = { "nvim-telescope/telescope.nvim", "nvim-lua/plenary.nvim", "nvim-telescope/telescope-file-browser.nvim" },
  config = function()
    require "startup".setup({
      theme = "evil",
      hooks = {
        after_open = function ()
          vim.api.nvim_buf_set_name(0, "startup")
        end
      }
    })
  end
}
