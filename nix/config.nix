{ nixpkgs }:
{
  flakeName = "euler-events-hs";
  defaultPackageName = "euler-events-hs";
  exportPackages = [
    "euler-events-hs"
  ];

  shellTools =
    with nixpkgs; [
      cabal-fmt
    ];
  # shellAttrs = {
  #   withHoogle = false;
  # };
}