{
  haskellCompiler ? "ghc883"
}:
let
  inherit (import <nixpkgs> {}) fetchFromGitHub;
  # Date:   Mon May 18 19:30:42 2020 -0500
  nixpkgs = fetchFromGitHub {
    owner = "NixOS";
    repo = "nixpkgs-channels";
    rev = "0f5ce2fac0c726036ca69a5524c59a49e2973dd4";
    sha256 = "0nkk492aa7pr0d30vv1aw192wc16wpa1j02925pldc09s9m9i0r3";
  };

  # To avoid importing nixpkgs here
  makeOverridable = f: origArgs:
    let
      origRes = f origArgs;
    in
      origRes // { override = newArgs: f (origArgs // newArgs); };

  eulerBuild = makeOverridable (import ./nix/euler-build) {
    inherit nixpkgs;
    inherit haskellCompiler;
  };

  euler-events-hs-src = eulerBuild.allowedPaths {
    root =  ./.;
    paths = [
      ./euler-events-hs.cabal
      ./src
      ./test
    ];
  };
  euler-events-hs-overlay = eulerBuild.importOverlay ./nix/overlays/euler-events-hs.nix {
    src = euler-events-hs-src;
  };

  # for both dev and CI
  code-tools-overlay = eulerBuild.importOverlay ./nix/overlays/code-tools.nix { };
  # for dev only
  devtools-overlay = import ./nix/overlays/devtools.nix { };

  allUsedOverlays = [
    code-tools-overlay
    euler-events-hs-overlay
  ];

  pkgs = import nixpkgs {
    overlays = allUsedOverlays;
  };

  haskellPackagesTools =
    with pkgs.haskellPackages;
    [
      hlint
      cabal-fmt
      nixfmt
      stylish-haskell
    ];
  tools = [];

  mkShell = eulerBuild.mkShell {
    drvPath = ./default.nix;
    drvName = "euler-events-hs";
    inherit haskellPackagesTools;
    inherit tools;
  };
in {
  inherit pkgs;
  euler-events-hs = pkgs.eulerHaskellPackages.euler-events-hs;

  inherit euler-events-hs-overlay;
  overlay = eulerBuild.composeOverlays allUsedOverlays;

  inherit code-tools-overlay;
  inherit devtools-overlay;
  inherit mkShell;

  # TODO: (?) put in a separate repo together with ./nix/euler-build
  inherit eulerBuild;
}
