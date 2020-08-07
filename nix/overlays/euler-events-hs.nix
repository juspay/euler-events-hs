{
  eulerBuild
, src
}:
let inherit (eulerBuild) fetchFromGitHub;
    inherit (builtins) fromJSON readFile;

  sources = fromJSON (readFile ../sources.json);

  prometheus-haskell-repo = fetchGit sources.prometheus-haskell;

  prometheus-client-path = "${prometheus-haskell-repo}/prometheus-client";
  prometheus-proc-path = "${prometheus-haskell-repo}/prometheus-proc";
  prometheus-metrics-ghc-path = "${prometheus-haskell-repo}/prometheus-metrics-ghc";
  wai-middleware-prometheus-path = "${prometheus-haskell-repo}/wai-middleware-prometheus";
in
eulerBuild.mkEulerHaskellOverlay
  (self: super: hself: hsuper: {

    prometheus-client =
      eulerBuild.fastBuildExternal {
        drv = hself.callCabal2nix "prometheus-client" prometheus-client-path { };
      };

    prometheus-proc =
      eulerBuild.fastBuildExternal {
        drv = hself.callCabal2nix "prometheus-proc" prometheus-proc-path { };
      };

    prometheus-metrics-ghc =
      eulerBuild.fastBuildExternal {
        drv = hself.callCabal2nix "prometheus-metrics-ghc" prometheus-metrics-ghc-path { };
      };

    wai-middleware-prometheus =
      eulerBuild.fastBuildExternal {
        drv = hself.callCabal2nix "wai-middleware-prometheus" wai-middleware-prometheus-path { };
      };

    euler-events-hs =
      eulerBuild.fastBuild {
        drv = hself.callCabal2nix "euler-events-hs" src { };
      };
  })
