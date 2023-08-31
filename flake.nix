{
  description = "diploma thesis explainer website";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-23.05";
    nixpkgs-unstable.url = "github:nixos/nixpkgs/nixos-unstable";
    rust-overlay = {
      url = "github:oxalica/rust-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    flake-parts.url = "github:hercules-ci/flake-parts";
  };

  outputs = inputs:
    inputs.flake-parts.lib.mkFlake { inherit inputs; } {
      imports = [
        ./devshell.nix
        ./build.nix
        ./nixos-module.nix
        ./nixos-vm.nix
      ];

      systems = [ "aarch64-linux" "aarch64-darwin" ];
      perSystem = { pkgs, system, self', ... }: {
        formatter = pkgs.nixpkgs-fmt;
        packages.default = self'.packages.vm;
      };
    };
}
