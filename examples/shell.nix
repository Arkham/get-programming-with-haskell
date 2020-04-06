{ ... }:
let
  sources = import ./nix/sources.nix;
  nixpkgs = import sources.nixpkgs { };
  all-hies =
    import (fetchTarball "https://github.com/infinisil/all-hies/tarball/master")
    { };
in with nixpkgs;
stdenv.mkDerivation {
  name = "get-programming-with-haskell";
  buildInputs = [
    pkgs.hlint
    pkgs.cachix
    (all-hies.selection { selector = p: { inherit (p) ghc865; }; })
  ];
}
