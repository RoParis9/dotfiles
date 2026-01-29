return {
	"nvim-telescope/telescope.nvim",
	event = "VimEnter",
	branch = "0.1.x",
	dependencies = {
		"nvim-lua/plenary.nvim",
		{
			"nvim-telescope/telescope-fzf-native.nvim",
			build = "make",
			cond = function()
				return vim.fn.executable("make") == 1
			end,
		},
		{ "nvim-telescope/telescope-ui-select.nvim" },
		{ "nvim-tree/nvim-web-devicons", enabled = vim.g.have_nerd_font },
	},
	config = function()
		local telescope = require("telescope")
		local builtin = require("telescope.builtin")
		local actions = require("telescope.actions")

		-- Configure Telescope
		telescope.setup({
			defaults = {
				mappings = {
					i = {
						["<C-k>"] = actions.move_selection_previous,
						["<C-j>"] = actions.move_selection_next,
						["<C-l>"] = actions.select_default,
						["<C-u>"] = actions.preview_scrolling_up,
						["<C-d>"] = actions.preview_scrolling_down,
					},
					n = {
						["<C-k>"] = actions.move_selection_previous,
						["<C-j>"] = actions.move_selection_next,
						["<C-l>"] = actions.select_default,
					},
				},
				file_ignore_patterns = {
					"node_modules",
					".git",
					".venv",
					"__pycache__",
					".next",
					"dist",
					"build",
				},
				path_display = { "truncate" },
				winblend = 0,
				border = {},
				borderchars = { "─", "│", "─", "│", "╭", "╮", "╯", "╰" },
				color_devicons = true,
				use_less = true,
				set_env = { ["COLORTERM"] = "truecolor" },
			},
			pickers = {
				find_files = {
					hidden = true,

				},
				live_grep = {
					additional_args = function()
						return { "--hidden" }
					end,
				},
				grep_string = {
					additional_args = function()
						return { "--hidden" }
					end,
				},
			},
			extensions = {
				["ui-select"] = {
					require("telescope.themes").get_dropdown({
						winblend = 10,
						width = 0.5,
						previewer = false,
						shorten_path = false,
					}),
				},
			},
		})

		-- Load extensions
		pcall(telescope.load_extension, "fzf")
		pcall(telescope.load_extension, "ui-select")

		-- Keymaps
		local keymap = vim.keymap.set
		local opts = { noremap = true, silent = true }

		-- Search keymaps
		keymap("n", "<leader>ff", builtin.find_files, { desc = "Find files" })
		keymap("n", "<leader>fg", builtin.live_grep, { desc = "Live grep" })
		keymap("n", "<leader>fb", builtin.buffers, { desc = "Find buffers" })
		keymap("n", "<leader>fh", builtin.help_tags, { desc = "Help tags" })
		keymap("n", "<leader>fr", builtin.oldfiles, { desc = "Recent files" })
		keymap("n", "<leader>fk", builtin.keymaps, { desc = "Keymaps" })
		keymap("n", "<leader>fc", builtin.commands, { desc = "Commands" })
		keymap("n", "<leader>fs", builtin.grep_string, { desc = "Grep string" })
		keymap("n", "<leader>fd", builtin.diagnostics, { desc = "Diagnostics" })
		keymap("n", "<leader>fq", builtin.quickfix, { desc = "Quickfix" })
		keymap("n", "<leader>fl", builtin.loclist, { desc = "Location list" })
		keymap("n", "<leader>ft", builtin.treesitter, { desc = "Treesitter" })
		keymap("n", "<leader>f/", builtin.current_buffer_fuzzy_find, { desc = "Fuzzy find in buffer" })

		-- Resume last search
		keymap("n", "<leader>f<space>", builtin.resume, { desc = "Resume last search" })

		-- Git keymaps
		keymap("n", "<leader>gf", builtin.git_files, { desc = "Git files" })
		keymap("n", "<leader>gb", builtin.git_branches, { desc = "Git branches" })
		keymap("n", "<leader>gc", builtin.git_commits, { desc = "Git commits" })
		keymap("n", "<leader>gs", builtin.git_status, { desc = "Git status" })

		-- LSP keymaps
		keymap("n", "<leader>lr", builtin.lsp_references, { desc = "LSP references" })
		keymap("n", "<leader>ld", builtin.lsp_definitions, { desc = "LSP definitions" })
		keymap("n", "<leader>li", builtin.lsp_implementations, { desc = "LSP implementations" })
		keymap("n", "<leader>lt", builtin.lsp_type_definitions, { desc = "LSP type definitions" })
		keymap("n", "<leader>ls", builtin.lsp_document_symbols, { desc = "LSP document symbols" })
		keymap("n", "<leader>lw", builtin.lsp_workspace_symbols, { desc = "LSP workspace symbols" })
	end,
}
