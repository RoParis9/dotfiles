local opts = { noremap = true, silent = true }

-- Shorten function name
local keymap = vim.keymap.set

--Remap space as leader key
vim.g.mapleader = " "
vim.g.maplocalleader = " "

-- Modes
--   normal_mode = "n",
--   insert_mode = "i",
--   visual_mode = "v",
--   visual_block_mode = "x",
--   term_mode = "t",
--   command_mode = "c",

-- Normal --
-- Better window navigation
keymap("n", "<C-h>", "<C-w>h", opts)
keymap("n", "<C-j>", "<C-w>j", opts)
keymap("n", "<C-k>", "<C-w>k", opts)
keymap("n", "<C-l>", "<C-w>l", opts)

-- Window management
keymap('n', '<leader>v', '<C-w>v', opts) -- split window vertically
keymap('n', '<leader>h', '<C-w>s', opts) -- split window horizontally
keymap('n', '<leader>se', '<C-w>=', opts) -- make split windows equal width & height
keymap('n', '<leader>xs', ':close<CR>', opts) -- close current split window

-- Resize with arrows
keymap("n", "<C-Up>", ":resize +2<CR>", opts)
keymap("n", "<C-Down>", ":resize -2<CR>", opts)
keymap("n", "<C-Left>", ":vertical resize -2<CR>", opts)
keymap("n", "<C-Right>", ":vertical resize +2<CR>", opts)

-- Navigate buffers
-- Buffers
keymap('n', '<Tab>', ':bnext<CR>', opts)
keymap('n', '<S-Tab>', ':bprevious<CR>', opts)
keymap('n', '<leader>x', ':Bdelete!<CR>', opts) -- close buffer
keymap('n', '<leader>b', '<cmd> enew <CR>', opts) -- new buffer

-- Visual --
-- Stay in indent mode
keymap("v", "<", "<gv", opts)
keymap("v", ">", ">gv", opts)

-- Move text up and down
keymap("v", "<A-j>", ":m .+1<CR>==", opts)
keymap("v", "<A-k>", ":m .-2<CR>==", opts)
keymap("v", "p", '"_dP', opts)

-- Visual Block --
-- Move text up and down
keymap("x", "J", ":move '>+1<CR>gv-gv", opts)
keymap("x", "K", ":move '<-2<CR>gv-gv", opts)
keymap("x", "<A-j>", ":move '>+1<CR>gv-gv", opts)
keymap("x", "<A-k>", ":move '<-2<CR>gv-gv", opts)

-- Terminal --
-- Better terminal navigation
keymap("t", "<C-h>", "<C-\\><C-N><C-w>h", term_opts)
keymap("t", "<C-j>", "<C-\\><C-N><C-w>j", term_opts)
keymap("t", "<C-k>", "<C-\\><C-N><C-w>k", term_opts)
keymap("t", "<C-l>", "<C-\\><C-N><C-w>l", term_opts)

-- Telescope
keymap("n", "<leader>t", "<cmd>lua require'telescope.builtin'.find_files(require('telescope.themes').get_dropdown({ previewer = false }))<cr>", opts)
keymap("n", "<c-t>", "<cmd>Telescope live_grep<cr>", opts)

-- debbugin
keymap("n", "<F5>", "<cmd>lua require'dap'.continue()<cr>", opts)
keymap("n", "<F11>", "<cmd>lua require'dap'.step_into()<cr>", opts)
keymap("n", "<F10>", "<cmd>lua require'dap'.step_over()<cr>", opts)
keymap("n", "<F12>", "<cmd>lua require'dap'.step_out()<cr>", opts)
keymap("n", "<leader>b", "<cmd>lua require'dap'.toggle_breakpoint()<cr>", opts)
keymap("n", "<leader>dr", "<cmd>lua require'dap'.repl.toggle()<cr>", opts)
keymap("n", "<leader>dl", "<cmd>lua require'dap'.run_last()<cr>", opts)
keymap("n", "<leader>du", "<cmd>lua require'dapui'.toggle()<cr>", opts)
keymap("n", "<leader>dt", "<cmd>lua require'dap'.terminate()<cr>", opts)

-- Close Buffer
keymap("n", "<S-q>", ":bdelete! <CR>", opts)

-- kill neovim
keymap("n", "<C-q>", "<cmd> q <CR>", opts)

-- Format
keymap("n","<leader>f",":Format<cr>", opts)

-- Save file
keymap("n","<C-s>","<cmd> w <CR>", opts)

-- Tabs
keymap('n', '<leader>to', ':tabnew<CR>', opts) -- open new tab
keymap('n', '<leader>tx', ':tabclose<CR>', opts) -- close current tab
keymap('n', '<leader>tn', ':tabn<CR>', opts) --  go to next tab
keymap('n', '<leader>tp', ':tabp<CR>', opts) --  go to previous tab

