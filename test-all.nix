# Nix configuration for testing bm against all supported GHC versions
#
# Usage:
#
#     $ nix-build test-all.nix

{
  bm-ghc-865 = import ./default.nix { compiler = "ghc865"; };
  bm-ghc-884 = import ./default.nix { compiler = "ghc884"; };
  bm-ghc-8107 = import ./default.nix { compiler = "ghc8107"; };
  bm-ghc-901 = import ./default.nix { compiler = "ghc901"; };
}
