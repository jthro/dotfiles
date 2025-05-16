require("telescope").setup({
  pickers = {
    find_files = {
      theme = "ivy",
      hidden = true
    },
    live_grep = {
      theme = "ivy"
    },
  }
})

vim.api.nvim_set_hl(0, "TelescopeBorder", { fg = "#4fd6be"})
vim.api.nvim_set_hl(0, "TelescopePromptBorder", { fg = "#4fd6be"})
vim.api.nvim_set_hl(0, "TelescopePromptTitle", { fg = "#4fd6be"})
