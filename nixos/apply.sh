#!/usr/bin/env bash
# Copy the patched NixOS home/flake files into /etc/nixos and rebuild.
set -euo pipefail

ROOT="$(cd "$(dirname "$0")" && pwd)"

echo "Installing patched home.nix and flake.nix into /etc/nixos ..."
sudo cp "$ROOT/home.nix" /etc/nixos/home.nix
sudo cp "$ROOT/flake.nix" /etc/nixos/flake.nix

echo "Updating my-dotfiles lock to the current ~/.dotfiles checkout ..."
sudo nix flake update my-dotfiles --flake /etc/nixos

echo "Rebuilding system (this installs Neovim LSP packages) ..."
sudo nixos-rebuild switch --flake /etc/nixos#nixos-acer

echo "Done. Restart Neovim, then check with: which gopls docker-langserver"
echo "In Neovim: :LspInfo"
