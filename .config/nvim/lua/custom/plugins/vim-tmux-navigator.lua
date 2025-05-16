return {
  "christoomey/vim-tmux-navigator",
  cmd = {
    "TmuxNavigateLeft",
    "TmuxNavigateDown",
    "TmuxNavigateUp",
    "TmuxNavigateRight",
    "TmuxNavigatePrevious",
    "TmuxNavigatorProcessList",
  },
  keys = {
    { "<leader>wh", "<cmd><C-U>TmuxNavigateLeft<cr>" },
    { "<leader>wj", "<cmd><C-U>TmuxNavigateDown<cr>" },
    { "<leader>wk", "<cmd><C-U>TmuxNavigateUp<cr>" },
    { "<leader>wl", "<cmd><C-U>TmuxNavigateRight<cr>" },
  },
}
