{ ... }: {
  perSystem = { pkgs, system, self', ... }: {
    devShells = {
      default = pkgs.mkShell {
        nativeBuildInputs = with pkgs; [
          nil
          nixpkgs-fmt
        ];

        inputsFrom = [ self'.packages.files ];
      };
    };
  };
}
