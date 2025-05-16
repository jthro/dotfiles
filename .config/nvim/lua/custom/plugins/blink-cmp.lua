return {
  "saghen/blink.cmp",
  version = "1.*",

  opts = {
    keymap = {
      preset = "default",
      ["<C-y>"] = {},
      ["<CR>"] = { "accept", "fallback" },
    },

    appearance = {
      use_nvim_cmp_as_default = true,
      nerd_font_variant = "mono",
    },

    signature = { enabled = true },
  },
}
