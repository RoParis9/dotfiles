 --vim.g.material_style="darker"

vim.cmd [[
 try
   colorscheme gruvbox 
 catch /^Vim\%((\a\+)\)\=:E185/
   colorscheme default
   set background=dark
 endtry
 ]]

