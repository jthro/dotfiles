-- TabBuffers
-- Maintain a list of buffers local to a tab

M = { }
M.tabs = { }

local maybe_init_tab = function (tab)
  if M.tabs[tab] == nil then
    M.tabs[tab] = { }
  end
end

M.tab_delete = function ()
  local tab = vim.api.nvim_get_current_tabpage()
  maybe_init_tab(tab)
  M.tabs[tab] = nil
end

M.buf_add = function (args)
  local tab = vim.api.nvim_get_current_tabpage()
  maybe_init_tab(tab)
  table.insert(M.tabs[tab], args.buf)
end

M.buf_remove = function (args)
  local tab = vim.api.nvim_get_current_tabpage()
  maybe_init_tab(tab)
  for i,v in ipairs(M.tabs[tab]) do
    if v == args.buf then
      table.remove(M.tabs[tab], i)
      break
    end
  end
end

M.list_bufs = function ()
  local tab = vim.api.nvim_get_current_tabpage()
  maybe_init_tab(tab)
  return M.tabs[tab]
end

vim.api.nvim_create_autocmd({"BufReadPost"}, {
  callback = M.buf_add,
})

vim.api.nvim_create_autocmd({"BufDelete"}, {
  callback = M.buf_remove,
})


local print_bufs = function ()
  local bufs = M.list_bufs()
  for _,v in ipairs(bufs) do
    print(v)
  end
end

vim.api.nvim_create_user_command("ListTabBufs", print_bufs, { desc = "List buffers belonging to this tab" })

return M
