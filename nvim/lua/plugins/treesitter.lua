return { -- Highlight, edit, and navigate code
  "nvim-treesitter/nvim-treesitter",
  -- Pin master: `main` is a full rewrite that dropped `nvim-treesitter.configs`
  -- (which this kickstart-style setup still uses).
  branch = "master",
  build = ":TSUpdate",
  main = "nvim-treesitter.configs", -- Sets main module to use for opts
  -- [[ Configure Treesitter ]] See `:help nvim-treesitter`
  opts = {
    ensure_installed = {
      "lua",
      "python",
      "javascript",
      "typescript",
      "regex",
      "sql",
      "dockerfile",
      "toml",
      "json",
      "go",
      "rust",
      "c",
      "cpp",
      "nix",
      "elixir",
      "ocaml",
      "gitignore",
      "yaml",
      "make",
      "cmake",
      "markdown",
      "markdown_inline",
      "bash",
      "tsx",
      "css",
      "html",
    },
    -- Autoinstall languages that are not installed
    auto_install = true,
    highlight = {
      enable = true,
    },
    indent = { enable = true, disable = { "ruby" } },
  },
}
