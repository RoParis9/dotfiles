 --vim.g.material_style="darker"

vim.cmd [[
 try
   colorscheme gruvbox 
 catch /^Vim\%((\a\+)\)\=:E185/
   colorscheme default
   set background=dark
 endtry
 ]]

function TransparentBackground()
  vim.api.nvim_set_hl(0,"Normal", {bg = "none"})
  vim.api.nvim_set_hl(0,"NormalFloat", {bg = "none"})
end

TransparentBackground()
