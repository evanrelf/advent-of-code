final: prev:

let
  inherit (prev) haskell-overlay;

in
haskell-overlay.mkOverlay
{
  extensions = [
    (haskell-overlay.sources (hfinal: hprev: {
      advent-of-code = ./.;
    }))
  ];
}
  final
  prev
