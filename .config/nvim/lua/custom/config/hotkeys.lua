local telescope = require("telescope")
local tbuiltin = require("telescope.builtin")
local tinyterm = require("custom.config.tinyterm")
local minibufremove = require("mini.bufremove")

-- Map hotkeys for LSP
-- Much shamelessly stolen from Kickstart
vim.api.nvim_create_autocmd("LspAttach", {
  group = vim.api.nvim_create_augroup("hk-lsp-attach", { clear = true }),
  callback = function(event)
    local map = function(keys, func, desc, mode)
      mode = mode or "n"
      vim.keymap.set(mode, keys, func, { buffer = event.buf, desc = "LSP: " .. desc })
    end

    map("<leader>rn", vim.lsp.buf.rename, "Rename")
    map("<leader>ca", vim.lsp.buf.code_action, "Code Action")
    map("<leader>cr", tbuiltin.lsp_references, "Find References")
    map("<leader>ci", tbuiltin.lsp_implementations, "Find Implementations")
    map("<leader>cd", tbuiltin.lsp_definitions, "Find Definition")
    map("<leader>cD", vim.lsp.buf.declaration, "Goto Declaration")
    map("<leader>ct", tbuiltin.lsp_type_definitions, "Goto Type Definition")
  end,
})

vim.keymap.set("n", "<leader>cq", vim.diagnostic.setloclist, { desc = "Open diagnostic [Q]uickfix list" })

-- Navigation
vim.keymap.set("n", "<leader>wh", "<C-w><C-h>", { desc = "Move focus to the left window" })
vim.keymap.set("n", "<leader>wl", "<C-w><C-l>", { desc = "Move focus to the right window" })
vim.keymap.set("n", "<leader>wj", "<C-w><C-j>", { desc = "Move focus to the lower window" })
vim.keymap.set("n", "<leader>wk", "<C-w><C-k>", { desc = "Move focus to the upper window" })

vim.keymap.set("n", "<leader>wv", "<C-w><C-v>", { desc = "Split Vertical" })
vim.keymap.set("n", "<leader>ws", "<C-w><C-s>", { desc = "Split Horizontal" })
vim.keymap.set("n", "<leader>wc", "<C-w><C-c>", { desc = "Close Window" })
vim.keymap.set("n", "<leader>bk", minibufremove.unshow, { desc = "Kill buffer" })

-- Toggle inline warnings
local virtual_text_on = true
local toggle_virtual_text = function()
  if virtual_text_on then
    vim.diagnostic.hide()
    virtual_text_on = false
    print("turning off")
  else
    vim.diagnostic.show()
    virtual_text_on = true
    print("turning on")
  end
end

vim.keymap.set("n", "<leader>bf", function()
  require("conform").format({ bufnr = vim.api.nvim_get_current_buf() })
end, { desc = "Format buffer" })

vim.keymap.set("n", "<leader>ch", toggle_virtual_text, { desc = "Toggle inline warnings" })

-- Toggle LateX rendering
local latex_rendering_on = false
local toggle_latex_rendering = function()
  if latex_rendering_on then
    vim.cmd("Neorg render-latex disable")
    latex_rendering_on = false
  else
    vim.cmd("Neorg render-latex")
    latex_rendering_on = true
  end
end

vim.keymap.set("n", "<leader>ml", toggle_latex_rendering, { desc = "Toggle LateX" })

-- Telescope
vim.keymap.set("n", "<leader>.", tbuiltin.find_files, { desc = "Search files" })
vim.keymap.set("n", "<leader>sp", tbuiltin.live_grep, { desc = "Search by grep" })
vim.keymap.set("n", "<leader>ff", function()
  vim.cmd("Oil")
end, { desc = "Search by grep" })

-- Terminal
vim.keymap.set("n", "<leader>ot", tinyterm.toggle_terminal)
vim.keymap.set("n", "<leader>oT", vim.cmd.term)
vim.keymap.set("t", "<Esc><Esc>", "<C-\\><C-n>", { noremap = true })

-- Open config
vim.keymap.set("n", "<leader>fP", ":tcd ~/dotfiles/<cr>", { desc = "Open dotfiles"})
