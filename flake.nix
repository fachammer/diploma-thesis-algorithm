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

  outputs = inputs@{ self, nixpkgs, nixpkgs-unstable, rust-overlay, flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      flake = {
        nixosModules.default = { config, pkgs, lib, ... }:
          with lib;
          let cfg = config.services.diploma-thesis-algorithm;
          in {
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

                files = mkOption {
                  type = types.package;
                  default = self.packages.${pkgs.system}.files;
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
                      "/".root = "${cfg.files}";
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
      };
      systems = [ "aarch64-linux" "aarch64-darwin" ];
      perSystem = { pkgs, system, ... }: {
        _module.args.pkgs = import inputs.nixpkgs {
          inherit system;
          overlays = [
            (import rust-overlay)
            (final: prev: {
              wasm-bindgen-cli = inputs.nixpkgs-unstable.legacyPackages.${system}.wasm-bindgen-cli;
            })
          ];
        };

        devShells = {
          default = pkgs.mkShell {
            buildInputs = with pkgs; [
              nil
              nixpkgs-fmt
            ];

            inputsFrom = [ self.packages.${system}.files ];
          };
        };

        formatter = pkgs.nixpkgs-fmt;

        packages =
          let
            systemArchitecture = builtins.head (builtins.split "-" system);
            guestSystem = "${systemArchitecture}-linux";
            vmNixosSystem =
              nixpkgs.lib.nixosSystem {
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
                    virtualisation.host.pkgs = nixpkgs.legacyPackages.${system};
                    virtualisation.forwardPorts = [
                      {
                        from = "host";
                        host.port = 8080;
                        guest.port = config.services.diploma-thesis-algorithm.port;
                      }
                    ];
                    services.getty.autologinUser = "root";

                    services.diploma-thesis-algorithm = {
                      enable = true;
                      port = 80;
                    };
                  })
                ];
              };
            vm = vmNixosSystem.config.system.build.vm;
          in
          {
            files =
              let
                rustBinary = pkgs.rust-bin.stable."1.69.0".default.override {
                  targets = [ "wasm32-unknown-unknown" ];
                };
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
            inherit vm;

            default = vm;
          };
      };
    };
}
