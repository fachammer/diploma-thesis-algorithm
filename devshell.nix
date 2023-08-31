{ ... }: {
  perSystem = { pkgs, system, self', ... }: {
    devShells = {
      default = pkgs.mkShell {
        buildInputs = with pkgs; [
          nil
          nixpkgs-fmt
        ];

        inputsFrom = [ self'.packages.files ];
      };
    };
  };
}
