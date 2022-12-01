let
  pkgs = import <nixpkgs> { };

in
pkgs.mkShell {
  buildInputs = [
    pkgs.cabal-install
    (pkgs.ghc.withPackages (p: [
      p.bytestring
      p.containers
      p.generic-lens
      p.lens
      p.megaparsec
      p.relude
      p.streamly
      p.text
      p.vector
    ]))
    pkgs.ghcid
  ];
}
