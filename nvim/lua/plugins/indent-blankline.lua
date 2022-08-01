require("indent_blankline").setup{
  char = ".",
  buftype_exclude={"terminal"},
  filetype_exclude={"dashboard","NvimTree","packer"},
  space_char_blankline = " ", 
  show_current_context = true,
  show_current_context_start = true,
}
