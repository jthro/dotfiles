local harpoon = require("harpoon")
harpoon:setup({})

local pickers = require("telescope.pickers")
local themes = require("telescope.themes")
local conf = require("telescope.config").values
local finders = require("telescope.finders")

local function toggle_telescope(harpoon_files)
    local file_paths = {}
    for _, item in ipairs(harpoon_files.items) do
        table.insert(file_paths, item.value)
    end

    pickers.new(themes.get_ivy(), {
        prompt_title = "Harpoon",
        finder = finders.new_table({
            results = file_paths,
        }),
        previewer = conf.file_previewer({}),
        sorter = conf.generic_sorter({}),
    }):find()
end

vim.keymap.set("n", "<leader>ha", function() harpoon:list():add() end)
vim.keymap.set("n", "<leader>pb", function() toggle_telescope(harpoon:list()) end)
