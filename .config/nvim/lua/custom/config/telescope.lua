local telescope = require("telescope")
local pickers = require("telescope.pickers")
local finders = require("telescope.finders")
local conf = require("telescope.config").values
local actions = require("telescope.actions")
local action_state = require("telescope.actions.state")
local themes = require("telescope.themes")


 telescope.setup({
  pickers = {
    find_files = {
      theme = "ivy",
      hidden = true,
    },
    live_grep = {
      theme = "ivy",
    },
    buffers = {
      theme = "ivy",
    },
  },
})

vim.api.nvim_set_hl(0, "TelescopeBorder", { fg = "#4fd6be" })
vim.api.nvim_set_hl(0, "TelescopePromptBorder", { fg = "#4fd6be" })
vim.api.nvim_set_hl(0, "TelescopePromptTitle", { fg = "#4fd6be" })

local function fd_find_dir()
  pickers.new(themes.get_ivy(), {
    prompt_title = "Find project",
    finder = finders.new_oneshot_job(
      { "fd", "--type", "d", "--hidden", "--exclude", ".git", ".", vim.fn.expand("$HOME/Projects/")},
      { entry_maker = require("telescope.make_entry").gen_from_file() }
    ),
    sorter = conf.generic_sorter(),
    attach_mappings = function(prompt_bufnr)
      actions.select_default:replace(function()
        actions.close(prompt_bufnr)
        local selection = action_state.get_selected_entry()
        vim.cmd("tcd "..vim.fn.fnameescape(selection[1]))
      end)
      return true
    end,
  }):find()
end
vim.keymap.set("n", "<leader>pp", fd_find_dir, { desc = "Fuzzy find project " })

