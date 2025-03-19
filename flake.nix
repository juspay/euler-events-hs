{
  inputs = {
    common.url = "github:juspay/nix-common/98c6ae8b431b1008fc41ff20fc1cb64037e4ef5b";
  };
  outputs = inputs:
    inputs.common.lib.mkFlake { inherit inputs; } {
      perSystem = { self', pkgs, pkgs-latest, config, filter, ... }: {

        haskellProjects.default = let fs = pkgs-latest.lib.fileset; in {
          projectRoot = builtins.toString (fs.toSource {
            root = ./.;
            fileset = fs.unions [
              ./src
              ./test
              ./euler-events-hs.cabal
            ];
          });

          packages = {
            # Dependencies
          };

          autoWire = [ "packages" ];
        };

        packages.default = self'.packages.euler-events-hs;

        devShells.default = pkgs.mkShell {
          name = "euler-events-hs";
          inputsFrom = [
            config.haskellProjects.default.outputs.devShell
            config.devShells.common
          ];
        };
      };

    };
}
