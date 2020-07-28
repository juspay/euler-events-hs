{
  remoteDeps ? false
, haskellCompiler ? "ghc883"
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

  euler-hs-repo = fetchGit {
    url = "git@bitbucket.org:juspay/euler-hs.git";
    ref = "EulerHS-1.11.1.0";
    # we take the last commit from the unannotated tag
    # rev = "4d939c18aef36f65e3afe1a80026c10555d95380";
  };
  euler-hs-path =
    if remoteDeps
    then euler-hs-repo
    else ../euler-hs;

 euler-hs-drv = import euler-hs-path {
    # Uncomment if you want to change haskellCompiler
    # that is used by euler-hs by default:
    # inherit haskellCompiler;
  };

  inherit (euler-hs-drv) eulerBuild;

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

  allUsedOverlays = [
    euler-hs-drv.code-tools-overlay
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

  inherit mkShell;
}
