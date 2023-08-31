{ inputs, ... }: {

  perSystem = { pkgs, system, ... }: {
    _module.args.pkgs = import inputs.nixpkgs {
      inherit system;
      overlays = [
        (import inputs.rust-overlay)
        (final: prev: {
          wasm-bindgen-cli = inputs.nixpkgs-unstable.legacyPackages.${system}.wasm-bindgen-cli;
        })
      ];
    };

    packages.files =
      let
        rustBinary = pkgs.rust-bin.fromRustupToolchainFile ./rust-toolchain.toml;
        rustPlatform = pkgs.makeRustPlatform {
          cargo = rustBinary;
          rustc = rustBinary;
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
  };

}
