return {
  "folke/tokyonight.nvim",
  lazy = false,
  priority = 1000,
  opts = {},
  config = function()

-- Function to toggle transparent background
local transparent = false

function ToggleTransparency()
  if transparent then
    require('tokyonight').setup({
      transparent = false,
    })
  else
    require('tokyonight').setup({
      transparent = true,
    })
  end
  transparent = not transparent
  vim.cmd("colorscheme tokyonight")
end

-- Keymap to toggle transparency
vim.api.nvim_set_keymap('n', '<leader>tt', ':lua ToggleTransparency()<CR>', { noremap = true, silent = true })

end,

}
