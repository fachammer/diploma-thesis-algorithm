{ inputs, self, ... }: {
  perSystem = { pkgs, system, ... }: {
    packages.vm =
      let
        systemArchitecture = builtins.head (builtins.split "-" system);
        guestSystem = "${systemArchitecture}-linux";
        vmNixosConfiguration = inputs.nixpkgs.lib.nixosSystem {
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
              virtualisation.host.pkgs = inputs.nixpkgs.legacyPackages.${system};
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
      in
      vmNixosConfiguration.config.system.build.vm;
  };
}
