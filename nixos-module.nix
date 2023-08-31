{ self, ... }: {
  flake.nixosModules.default = { config, pkgs, lib, ... }:
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
}
