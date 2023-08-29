{
  description = "diploma thesis explainer website";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-23.05";
  };

  outputs = { nixpkgs, ... }:
    let
      mapPerSystemPkgs = f: builtins.mapAttrs f nixpkgs.legacyPackages;
    in
    {
      devShells = mapPerSystemPkgs
        (system: pkgs: {
          default = pkgs.mkShell {
            buildInputs = with pkgs; [
              nil
              nixpkgs-fmt
              cowsay
            ];
          };
        });

      formatter = mapPerSystemPkgs
        (system: pkgs: pkgs.nixpkgs-fmt);

      packages = mapPerSystemPkgs
        (system: pkgs: {
          default = { };
        });
    };
}
