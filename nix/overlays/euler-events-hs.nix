{
  eulerBuild
, src
}:
eulerBuild.mkEulerHaskellOverlay
  (self: super: hself: hsuper: {
    euler-events-hs =
      eulerBuild.fastBuild {
        drv = hself.callCabal2nix "euler-events-hs" src { };
      };

    euler-events-hs-with-tests =
      eulerBuild.fastBuild {
        drv = hself.callCabal2nix "euler-events-hs" src { };
        overrides = {
          runTests = true;
        };
      };
  })
