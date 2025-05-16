M = { }

M.toggle_terminal = function()
  -- get current state for this tab
  local term_buf = vim.t.mini_term_buf
  local term_win = vim.t.mini_term_win

  --- show the terminal by creating the window and attaching
  --- to the buffer
  -- @param init: if non-nil will create the term
  -- @param size: set the height, default 15
  local show_terminal = function (init)
    vim.cmd("vsplit")
    term_win = vim.api.nvim_get_current_win()
    vim.t.mini_term_win = term_win
    vim.api.nvim_win_set_buf(0, term_buf)
    vim.cmd.wincmd("J")
    vim.api.nvim_win_set_height(0, 15)
    if init ~= nil then vim.cmd.term() end
  end

  --- hide the terminal by closing the window
  local hide_terminal = function ()
    vim.api.nvim_set_current_win(term_win)
    vim.cmd("close")
  end

  if term_buf == nil or not vim.api.nvim_buf_is_valid(term_buf) then
    -- no term_buf yet: create it, name it, then show the window 
    -- creating the term itself
    term_buf = vim.api.nvim_create_buf(true, true)
    vim.t.mini_term_buf = term_buf
    show_terminal(true)

  elseif term_win ~= nil and vim.api.nvim_win_is_valid(term_win) then
    -- buf exists, win open: close it
    hide_terminal()
  else
    -- buf exists, no win open: open it
    show_terminal()
  end
end

return M
