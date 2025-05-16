return {
  "nanozuki/tabby.nvim",
  config = function ()
    vim.o.showtabline = 2

    vim.api.nvim_set_keymap("n", "<leader><tab>n", ":$tabnew<CR>", { noremap = true })
    vim.api.nvim_set_keymap("n", "<leader><tab>d", ":$tabclose<CR>", { noremap = true })
    vim.api.nvim_set_keymap("n", "<leader><tab>l", "gt", { noremap = true })
    vim.api.nvim_set_keymap("n", "<leader><tab>h", "gT", { noremap = true })
    vim.api.nvim_set_keymap("n", "<leader><tab>tr", ":Tabby rename_tab ", { noremap = true })

    local theme = {
      fill = "TabLineFill",
      head = "TabLine",
      current_tab = "TabLineSel",
      tab = "TabLine",
      win = "TabLine",
      tail = "TabLine"
    }
    require('tabby').setup({
      line = function(line)
        return {
          {
            { '  ', hl = theme.head },
          },
          line.tabs().foreach(function(tab)
            local hl = tab.is_current() and theme.current_tab or theme.tab
            return {
              line.sep('', hl, theme.fill),
              tab.number(),
              tab.name(),
              line.sep('', hl, theme.fill),
              hl = hl,
              margin = ' ',
            }
          end),
        line.spacer(),
        line.wins_in_tab(line.api.get_current_tab()).foreach(function(win)
          return {
            win.is_current() and '' or '',
            win.buf_name(),
            hl = theme.win,
            margin = ' ',
          }
        end),
        {
          { '  ', hl = theme.tail },
        },
        hl = theme.fill,
      }
    end
    })
  end
}
