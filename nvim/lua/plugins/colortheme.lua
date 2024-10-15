return {
	"ellisonleao/gruvbox.nvim",
	lazy = false, -- load on startup
	priority = 1000, -- ensure it loads before other plugins
	config = function()
		vim.cmd([[colorscheme gruvbox]])
	end,
}
