local telescope = require("telescope")
local pickers = require("telescope.pickers")
local finders = require("telescope.finders")
local conf = require("telescope.config").values
local actions = require("telescope.actions")
local action_state = require("telescope.actions.state")
local themes = require("telescope.themes")
local previewers = require("telescope.previewers")
local tabbuffers = require("custom.config.tabbuffers")


 telescope.setup({
  pickers = {
    find_files = {
      theme = "ivy",
      hidden = true,
    },
    live_grep = {
      theme = "ivy",
      hidden = true,
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

local function buffers_pick(title, bufnrs)
  pickers.new(themes.get_ivy(), {
    prompt_title = title,
    finder = finders.new_table {
      results = bufnrs,
      entry_maker = function (bufnr)
        local name = vim.api.nvim_buf_get_name(bufnr)
        if name == "" then name = "[No Name]" end
        return {
          value = bufnr,
          display = name,
          ordinal = name,
          bufnr = bufnr,
          filename = name,
        }
      end,
    },
    sorter = conf.generic_sorter({}),
    previewer = previewers.new_buffer_previewer {
      define_preview = function (self, entry, _)
        vim.fn.bufload(entry.bufnr)
        local lines = vim.api.nvim_buf_get_lines(entry.bufnr, 0, -1, false)
        vim.api.nvim_buf_set_lines(self.state.bufnr, 0, -1, false, lines)
        vim.api.nvim_set_option_value("filetype",
          vim.api.nvim_get_option_value("filetype", { buf = entry.bufnr }),
          { buf = self.state.bufnr })
      end
    },
    attach_mappings = function (prompt_bufnr, _)
      actions.select_default:replace(function ()
        actions.close(prompt_bufnr)
        local selection = action_state.get_selected_entry()
        vim.api.nvim_set_current_buf(selection.value.bufnr)
      end)
      return true
    end,
  }):find()
end

local get_tab_buffers = function ()
  buffers_pick("Tab Buffers", tabbuffers.list_bufs())
end

vim.keymap.set("n", "<leader>BB", get_tab_buffers, { desc = "Pick from tab buffers" })
