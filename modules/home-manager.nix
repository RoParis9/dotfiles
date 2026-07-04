# Home Manager module: symlinks dotfiles from this repo into $HOME.
#
# Enable in your NixOS / standalone Home Manager config:
#
#   inputs.dotfiles.url = "github:RoParis9/dotfiles";
#
#   # inside home-manager { ... }:
#   imports = [ inputs.dotfiles.homeModules.default ];
#   dotfiles.enable = true;
#   dotfiles.source = inputs.dotfiles;   # path to this flake checkout
#
# Toggle individual apps with dotfiles.<app>.enable (all default to true).
dotfilesSource: # Flake root passed from flake.nix (curried as the first argument).
{
  lib, # Home Manager / Nixpkgs helper functions.
  config, # The user's full Home Manager configuration.
  ...
}:
let
  # Shorthand for option / conditional helpers.
  inherit (lib) mkIf mkEnableOption mkOption types mkMerge;

  # Resolved configuration namespace: `config.dotfiles.*`.
  cfg = config.dotfiles;
in
{
  # ── Options ───────────────────────────────────────────────────────────────
  options.dotfiles = {
    # Master switch; when false, this module installs nothing.
    enable = mkEnableOption "RoParis9 dotfiles";

    # Path to the dotfiles source tree (flake checkout or local path).
    source = mkOption {
      type = types.path;
      default = dotfilesSource;
      description = "Root directory of the dotfiles repository.";
    };

    # Per-application toggles so you can mix these dotfiles with other configs.
    emacs = {
      enable = mkEnableOption "Emacs init.el from dotfiles";
    };

    neovim = {
      enable = mkEnableOption "Neovim configuration from dotfiles";
    };

    zsh = {
      enable = mkEnableOption ".zshrc from dotfiles";
    };

    tmux = {
      enable = mkEnableOption ".tmux.conf from dotfiles";
    };

    alacritty = {
      enable = mkEnableOption "Alacritty config from dotfiles";
    };

    wofi = {
      enable = mkEnableOption "Wofi style from dotfiles";
    };
  };

  # ── Configuration ─────────────────────────────────────────────────────────
  config = mkIf cfg.enable (
    mkMerge [
      # Emacs: init.el lives in ~/.emacs.d/ by default.
      (mkIf cfg.emacs.enable {
        home.file.".emacs.d/init.el".source = "${cfg.source}/init.el";
      })

      # Neovim: entire nvim/ directory → ~/.config/nvim/.
      (mkIf cfg.neovim.enable {
        xdg.configFile.nvim.source = "${cfg.source}/nvim";
      })

      # Zsh: top-level rc file.
      (mkIf cfg.zsh.enable {
        home.file.".zshrc".source = "${cfg.source}/.zshrc";
      })

      # Tmux: top-level config file.
      (mkIf cfg.tmux.enable {
        home.file.".tmux.conf".source = "${cfg.source}/.tmux.conf";
      })

      # Alacritty: TOML config under XDG config dir.
      (mkIf cfg.alacritty.enable {
        xdg.configFile."alacritty/alacritty.toml".source = "${cfg.source}/alacritty.toml";
      })

      # Wofi: launcher CSS theme.
      (mkIf cfg.wofi.enable {
        xdg.configFile."wofi/style.css".source = "${cfg.source}/wofi/style.css";
      })
    ]
  );
}
