-- vim.g.tokyonight_style="storm"
vim.g.material_style="darker"

vim.cmd [[
 try
   colorscheme material
 catch /^Vim\%((\a\+)\)\=:E185/
   colorscheme default
   set background=dark
 endtry
 ]]
