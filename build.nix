{ devShell ? false }:
let
  nix-inclusive = builtins.fetchTarball "https://github.com/juspay/nix-inclusive/archive/2ca1706029bfcf4bb7eaf17b4f32e49f436a148e.tar.gz";
  filter = import "${nix-inclusive}/inclusive.nix" { lib = (import <nixpkgs> {}).lib; };
  path =
    filter ./. [
      ./euler-events-hs.cabal
      ./src
      ./test
      ./flake.nix
      ./flake.lock
      ./nix/overlay.nix
    ];
  attr = if devShell then "devShell" else "defaultPackage";
in
(builtins.getFlake (toString (builtins.unsafeDiscardStringContext path))).${attr}.${builtins.currentSystem}
