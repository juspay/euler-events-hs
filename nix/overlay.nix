# Autogenerated from euler.yaml. Do not edit.
# hash: cdefbb9ead683556631523513dbe5a3765eea647f3050b8c439d4b825dc73beb
# time: 2020-10-27 15:13:47.386652

self: super:
let
  euler-events-hs-src = super.eulerBuild.allowedPaths {
    root = ./..;
    paths = [
      ../euler-events-hs.cabal
      ../src
      ../test
    ];
  };

  prometheus-haskell-repo = builtins.fetchTarball {
    url = "https://github.com/juspay/prometheus-haskell/archive/7bf933ad3e0059020273ab9d7fc799c582d663ae.tar.gz";
    sha256 = "1a00yb7258gk73idwffd2c1fvw6jci71mal143q81pgxaq9ivf78";
  };
  prometheus-client-path = "${prometheus-haskell-repo}/prometheus-client";
prometheus-proc-path = "${prometheus-haskell-repo}/prometheus-proc";
prometheus-metrics-ghc-path = "${prometheus-haskell-repo}/prometheus-metrics-ghc";
wai-middleware-prometheus-path = "${prometheus-haskell-repo}/wai-middleware-prometheus";




in 
super.eulerBuild.mkEulerHaskellOverlay self super
  (hself: hsuper: {
    prometheus-client = self.eulerBuild.fastBuildExternal {
      drv = super.haskell.lib.unmarkBroken (hself.callCabal2nix "prometheus-client" prometheus-client-path { });
    };
prometheus-proc = self.eulerBuild.fastBuildExternal {
      drv = super.haskell.lib.unmarkBroken (hself.callCabal2nix "prometheus-proc" prometheus-proc-path { });
    };
prometheus-metrics-ghc = self.eulerBuild.fastBuildExternal {
      drv = super.haskell.lib.unmarkBroken (hself.callCabal2nix "prometheus-metrics-ghc" prometheus-metrics-ghc-path { });
    };
wai-middleware-prometheus = self.eulerBuild.fastBuildExternal {
      drv = super.haskell.lib.unmarkBroken (hself.callCabal2nix "wai-middleware-prometheus" wai-middleware-prometheus-path { });
    };
stm-containers = self.eulerBuild.fastBuildExternal {
      drv = super.haskell.lib.unmarkBroken (hsuper.stm-containers);
    };
list-t = self.eulerBuild.fastBuildExternal {
      drv = super.haskell.lib.unmarkBroken (hsuper.list-t);
    };
stm-hamt = self.eulerBuild.fastBuildExternal {
      drv = super.haskell.lib.unmarkBroken (hsuper.stm-hamt);
    };
primitive-extras = self.eulerBuild.fastBuildExternal {
      drv = super.haskell.lib.unmarkBroken (hsuper.primitive-extras);
    };
    
    euler-events-hs = self.eulerBuild.fastBuild {
      drv = hself.callCabal2nix "euler-events-hs" euler-events-hs-src { };
      overrides = {
        # We want to run tests for our packages most of the time
        runTests = true;
      };
    };
  })
