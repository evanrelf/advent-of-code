let
  pkgs = import <nixpkgs> { };

  advent-of-code = pkgs.haskellPackages.callCabal2nix "advent-of-code" ./. { };

in
advent-of-code.env.overrideAttrs (prev: {
  buildInputs = [
    pkgs.cabal-install
    pkgs.ghcid
  ];
})
