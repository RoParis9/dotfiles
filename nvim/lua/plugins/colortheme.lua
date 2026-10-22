-- Keep syntax colors from the active theme; clear background so the
-- terminal's transparency/blur shows through.
local function apply_transparent_bg()
    local groups = {
        "Normal",
        "NormalNC",
        "NormalFloat",
        "FloatBorder",
        "SignColumn",
        "EndOfBuffer",
        "LineNr",
        "CursorLine",
        "CursorLineNr",
        "Folded",
        "FoldColumn",
        "NonText",
        "SpecialKey",
        "VertSplit",
        "WinSeparator",
        "StatusLine",
        "StatusLineNC",
        "TabLine",
        "TabLineFill",
        "NeoTreeNormal",
        "NeoTreeNormalNC",
        "TelescopeNormal",
        "TelescopeBorder",
        "TelescopePromptNormal",
        "TelescopeResultsNormal",
        "TelescopePreviewNormal",
    }

    for _, group in ipairs(groups) do
        local ok, hl = pcall(vim.api.nvim_get_hl, 0, { name = group })
        if ok and hl then
            hl.bg = nil
            hl.ctermbg = nil
            vim.api.nvim_set_hl(0, group, hl)
        else
            vim.api.nvim_set_hl(0, group, { bg = "NONE" })
        end
    end
end

vim.api.nvim_create_autocmd("ColorScheme", {
    group = vim.api.nvim_create_augroup("TransparentBackground", { clear = true }),
    callback = apply_transparent_bg,
})

return {
    -- Gruvbox (versão padrão - elisonleao)
    {
        "ellisonleao/gruvbox.nvim",
        lazy = false,
        priority = 1000,
        opts = { transparent_mode = true },
    },

    -- Dracula
    {
        "Mofiqul/dracula.nvim",
        lazy = false,
        priority = 1000,
        opts = { transparent_bg = true },
    },

    -- TokyoNight
    {
        "folke/tokyonight.nvim",
        lazy = false,
        priority = 1000,
        opts = {
            style = "night",
            transparent = true,
            styles = {
                sidebars = "transparent",
                floats = "transparent",
            },
        },
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
        opts = {
            flavor = "mocha",
            transparent_background = true,
        },
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
                    require("lualine").setup({
                        options = { theme = "auto" },
                    })
                    apply_transparent_bg()
                end,

                globalBefore = [[
                    vim.o.background = 'dark'
                ]],
            })

            vim.keymap.set("n", "<leader>T", "<Cmd>Themery<CR>", { desc = "Abrir Menu de Temas" })
            apply_transparent_bg()
        end,
    },
}
