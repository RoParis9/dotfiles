return {
  {'akinsho/toggleterm.nvim', version = "*", opts = {
    -- Terminal configuration
    size = function(term)
      if term.direction == "horizontal" then
        return 15
      elseif term.direction == "vertical" then
        return vim.o.columns * 0.4
      end
    end,
    open_mapping = [[<c-\>]],
    hide_numbers = true,
    shade_filetypes = {},
    shade_terminals = true,
    shading_factor = 2,
    start_in_insert = true,
    insert_mappings = true,
    persist_size = true,
    direction = "horizontal",
    close_on_exit = true,
    shell = vim.o.shell,
    float_opts = {
      border = "curved",
      winblend = 0,
      highlights = {
        border = "Normal",
        background = "Normal",
      },
    },
  },
  config = function(_, opts)
    require("toggleterm").setup(opts)
    
    -- Keymaps for terminal
    local Terminal = require("toggleterm.terminal").Terminal
    
    -- Create a horizontal terminal that opens at the bottom
    local horizontal_term = Terminal:new({
      direction = "horizontal",
      size = 15,
    })
    
    -- Toggle horizontal terminal with <leader>t
    vim.keymap.set("n", "<leader>t", function()
      horizontal_term:toggle()
    end, { desc = "Toggle horizontal terminal" })
    
    -- Toggle terminal in terminal mode
    vim.keymap.set("t", "<leader>t", function()
      horizontal_term:toggle()
    end, { desc = "Toggle horizontal terminal" })
    
    -- Optional: Create a floating terminal with <leader>tf
    local float_term = Terminal:new({
      direction = "float",
      float_opts = {
        border = "curved",
        winblend = 0,
        highlights = {
          border = "Normal",
          background = "Normal",
        },
      },
    })
    
    vim.keymap.set("n", "<leader>tf", function()
      float_term:toggle()
    end, { desc = "Toggle floating terminal" })
  end,
  }
}
