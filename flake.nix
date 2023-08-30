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

          vm-aarch64-linux = (self.nixosConfigurations.vm {
            guestSystem = "aarch64-linux";
            hostSystem = system;
          }).config.system.build.vm;
        });

      nixosModules.default = { config, pkgs, lib, ... }:
        with lib;
        let cfg = config.services.diploma-thesis-algorithm; in {
          options = {
            services.diploma-thesis-algorithm = {
              enable = mkEnableOption "Enables the diploma-thesis-algorithm service";

              port = mkOption {
                type = types.port;
                default = 80;
              };

              address = mkOption {
                type = types.str;
                default = "0.0.0.0";
              };

              package = mkOption {
                type = types.package;
                default = self.packages.${pkgs.system}.default;
              };
            };
          };

          config = mkIf cfg.enable {
            services.nginx = {
              enable = true;
              appendHttpConfig = ''
                error_log stderr;
                access_log syslog:server=unix:/dev/log combined;
              '';
              virtualHosts = {
                "${cfg.address}" = {
                  listen = [{
                    inherit (cfg) port;
                    addr = cfg.address;
                  }];
                  locations = {
                    "/".root = "${cfg.package}";
                  };
                };
              };
            };

            networking.firewall = {
              enable = true;
              allowedTCPPorts = [
                config.services.diploma-thesis-algorithm.port
              ];
            };
          };
        };

      nixosConfigurations.vm = { hostSystem, guestSystem }: nixpkgs.lib.nixosSystem {
        system = guestSystem;

        modules = [
          ({ config, modulesPath, ... }: {
            imports = [
              "${modulesPath}/virtualisation/qemu-vm.nix"
              self.nixosModules.default
            ];
            system.stateVersion = "23.05";
            nixpkgs.localSystem.system = guestSystem;

            virtualisation.diskImage = "./diploma-thesis-algorithm.qcow2";
            virtualisation.graphics = false;
            virtualisation.host.pkgs = nixpkgs.legacyPackages.${hostSystem};
            virtualisation.forwardPorts = [
              {
                from = "host";
                host.port = 8080;
                guest.port = config.services.diploma-thesis-algorithm.port;
              }
            ];

            services.getty.autologinUser = "root";

            networking.firewall = {
              enable = true;
              allowedTCPPorts = [
                config.services.diploma-thesis-algorithm.port
              ];
            };

            services.diploma-thesis-algorithm = {
              enable = true;
              port = 80;
            };
          })
        ];
      };
    };
}
