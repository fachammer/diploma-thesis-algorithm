{
  description = "diploma thesis explainer website";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-23.05";
    nixpkgsUnstable.url = "github:nixos/nixpkgs/master";
    rust-overlay.url = "github:oxalica/rust-overlay";
  };

  outputs = { self, nixpkgs, nixpkgsUnstable, rust-overlay, ... }:
    let
      mapPerSystemPkgs = f: builtins.mapAttrs
        (system: pkgs:
          let
            overlayedPkgs = import nixpkgs
              {
                inherit system;
                overlays = [
                  (import rust-overlay)
                  (final: prev: {
                    wasm-bindgen-cli = nixpkgsUnstable.legacyPackages.${system}.wasm-bindgen-cli;
                  })
                ];
              };
          in
          f system overlayedPkgs)
        nixpkgs.legacyPackages;
    in
    {
      devShells = mapPerSystemPkgs
        (system: pkgs: {
          default = pkgs.mkShell {
            buildInputs = with pkgs; [
              nil
              nixpkgs-fmt
            ];

            inputsFrom = [ self.packages.${system}.default ];
          };
        });

      formatter = mapPerSystemPkgs
        (system: pkgs: pkgs.nixpkgs-fmt);

      packages = mapPerSystemPkgs
        (system: pkgs: {
          default =
            let
              rust-binary = pkgs.rust-bin.fromRustupToolchainFile ./rust-toolchain.toml;
              rustPlatform = pkgs.makeRustPlatform {
                cargo = rust-binary;
                rustc = rust-binary;
              };
            in
            rustPlatform.buildRustPackage {
              name = "diploma-thesis-algorithm";
              src = ./.;
              cargoLock = {
                lockFile = ./Cargo.lock;
              };
              nativeBuildInputs = with pkgs; [
                wasm-bindgen-cli
              ];
              buildPhase = ''
                set -ex
                cargo build --target wasm32-unknown-unknown --release
                mkdir -p ./www/pkg
                wasm-bindgen --target web --out-dir ./www/pkg --no-typescript ./target/wasm32-unknown-unknown/release/thesis_algorithm.wasm
              '';
              installPhase = ''
                cp -R ./www/. $out
              '';
            };
        });
    };
}
