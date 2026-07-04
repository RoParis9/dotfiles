# Nix flake entry point for these dotfiles.
# A flake is a reproducible, lockable Nix project you can consume from other configs.
{
  # Human-readable description shown by `nix flake metadata` and on flakehub/search.nixos.org.
  description = "Personal dotfiles (Neovim, Emacs, shell, terminal, etc.)";

  # External dependencies this flake needs. Each input is pinned by flake.lock.
  inputs = {
    # Official Nixpkgs channel; nixos-unstable gives recent packages.
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

    # Home Manager: declarative user environment (dotfiles, programs, services).
    home-manager.url = "github:nix-community/home-manager";

    # Reuse the same nixpkgs revision as this flake to avoid duplicate builds.
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
  };

  # What this flake exposes to consumers (your NixOS / Home Manager setup).
  outputs =
    {
      self, # This flake's source tree and metadata (rev, narHash, etc.).
      nixpkgs, # The nixpkgs input declared above.
      home-manager, # The home-manager input declared above.
      ...
    }@inputs:
    let
      # Helper: pick nixpkgs for a given system (x86_64-linux, aarch64-linux, …).
      forAllSystems =
        systems: f:
        nixpkgs.lib.genAttrs systems (
          system:
          f {
            inherit system;
            # Import nixpkgs for `system`, honoring this flake's nixConfig if any.
            pkgs = import nixpkgs {
              inherit system;
              overlays = [ ];
            };
          }
        );

      # Architectures this flake is tested / intended for.
      supportedSystems = [
        "x86_64-linux"
        "aarch64-linux"
      ];

      # Shared Home Manager module factory; avoids duplicating import logic.
      mkDotfilesHomeModule =
        {
          dotfilesSource ? self, # Optional override when importing from another path.
          ...
        }:
        import ./modules/home-manager.nix (dotfilesSource or self);
    in
    {
      # ── Home Manager modules ────────────────────────────────────────────────
      # Import one of these in your home-manager `imports` list.
      #
      # Example (inside your NixOS home-manager user block):
      #   imports = [ inputs.dotfiles.homeModules.default ];
      #   dotfiles.enable = true;
      #   dotfiles.source = inputs.dotfiles;
      homeModules = {
        # Primary module name; recommended import target.
        default = mkDotfilesHomeModule;

        # Alias with an explicit name if you prefer `homeModules.dotfiles`.
        dotfiles = mkDotfilesHomeModule;
      };

      # ── NixOS modules ─────────────────────────────────────────────────────
      # Thin wrapper that re-exports the Home Manager module for NixOS users
      # who import flake modules at the OS level (optional convenience).
      nixosModules = {
        default = {
          dotfilesSource ? self,
          ...
        }@moduleArgs:
        {
          # Makes `dotfilesSource` available to nested home-manager imports.
          _module.args.dotfilesSource = dotfilesSource or self;
        };
      };

      # ── Overlays (optional extension point) ────────────────────────────────
      # Add custom packages here later; empty for now.
      overlays = [ ];

      # ── Packages built from this flake ────────────────────────────────────
      # Useful for `nix build github:RoParis9/dotfiles#<attr>` smoke tests.
      packages = forAllSystems supportedSystems (
        { pkgs, ... }:
        {
          # Default package: just bundles the raw dotfiles tree.
          default = pkgs.stdenv.mkDerivation {
            pname = "dotfiles";
            version = self.shortRev or self.dirtyShortRev or "unknown";
            src = self;
            dontBuild = true;
            installPhase = ''
              mkdir -p $out
              cp -r $src/. $out/
            '';
          };
        }
      );

      # ── Formatter (optional quality-of-life) ──────────────────────────────
      # Run `nix fmt` in this repo to format all *.nix files.
      formatter = forAllSystems supportedSystems ({ pkgs, ... }: pkgs.nixfmt-rfc-style);

      # ── Checks (optional CI hook) ─────────────────────────────────────────
      # `nix flake check` evaluates these derivations.
      checks = forAllSystems supportedSystems (
        { system, ... }:
        {
          # Ensures the default package evaluates and builds.
          default = self.packages.${system}.default;
        }
      );
    };
}
