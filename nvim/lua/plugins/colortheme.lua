return {
    -- Gruvbox (versão padrão - elisonleao)
    { "ellisonleao/gruvbox.nvim", lazy = false, priority = 1000, },

    -- Dracula
    { "Mofiqul/dracula.nvim", lazy = false, priority = 1000, },

    -- TokyoNight
    {
        "folke/tokyonight.nvim",
        lazy = false,
        priority = 1000,
        opts = { style = "night" },
        config = function(_, opts)
            require("tokyonight").setup(opts)
        end,
    },

    -- Catppuccin
    {
        "catppuccin/nvim",
        name = "catppuccin",
        lazy = false,
        priority = 1000,
        opts = { flavor = "mocha" },
        config = function(_, opts)
            require("catppuccin").setup(opts)
        end,
    },
    

    -- Themery.nvim - O gerenciador de temas
    {
        "zaldih/themery.nvim",
        lazy = false,
        config = function()
            require("themery").setup({
                themes = {
                    "gruvbox",
                    "dracula",
                    "dracula-soft",
                    "tokyonight-night",
                    "tokyonight-storm",
                    "catppuccin-frappe",
                    "catppuccin-macchiato",
                    "catppuccin-mocha",
                },
                livePreview = true,
                themeDefault = "kanagawa-dragon",
                -- Add this hook to refresh lualine every time you pick a new theme
                on_change = function()
                    require('lualine').setup({
                        options = { theme = 'auto' }
                    })
                end,    

                globalBefore = [[
                    vim.o.background = 'dark'
                ]],
            })
            
            vim.keymap.set("n", "<leader>T", "<Cmd>Themery<CR>", { desc = "Abrir Menu de Temas" })
        end,
    },
}
