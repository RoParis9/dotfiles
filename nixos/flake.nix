{
  description = "Nixos Flake Setup with Auto-Fetching";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-26.05";
    home-manager = {
      url = "github:nix-community/home-manager/release-26.05";
      inputs.nixpkgs.follows = "nixpkgs";
    };


    zen-browser = {
      url = "github:youwen5/zen-browser-flake";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # Local checkout so home-manager can follow WIP module changes.
    my-dotfiles = {
      url = "path:/home/rodrigo/.dotfiles";
    };

  };

  outputs = { self, nixpkgs, home-manager, my-dotfiles, ... }@inputs: {
    nixosConfigurations.nixos-acer = nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";
     
      specialArgs = { inherit inputs; };
   
      modules = [
        ./configuration.nix
        home-manager.nixosModules.home-manager
        {
          home-manager = {
            useGlobalPkgs = true;
            useUserPackages = true;
            # Pass flake inputs into home.nix (needed for inputs.my-dotfiles).
            extraSpecialArgs = { inherit inputs; };
            users.rodrigo = import ./home.nix;
            backupFileExtension = "backup";
           };
        }
       ];
      };
    };
}
