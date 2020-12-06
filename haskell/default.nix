let
  haskellPackagesOverlay =
    import ./nix/override-haskell-packages.nix {
      packages = {
        "advent-of-code" = pkgs.nix-gitignore.gitignoreSource [ ./.nixignore ] ./.;
      };
      overrides = {};
      hackage = {
        rev = "52415450270fb5d146097c36e74d1117ba0e4fe4";
        sha256 = "0cc7ls5awhb83jfm8kcaskglgqala32q5s8j87frz0f6wx57gbl4";
      };
    };


  pkgs = import ./nix/nixpkgs.nix { overlays = [ haskellPackagesOverlay ]; };


  advent-of-code = pkgs.haskellPackages.advent-of-code;


  executable = pkgs.haskell.lib.justStaticExecutables advent-of-code;


  shell =
    advent-of-code.env.overrideAttrs (old: {
      buildInputs = with pkgs; old.buildInputs ++ [
        cabal-install
        ghcid
        hlint
      ];
    });

in
  { inherit
      advent-of-code
      executable
      shell
    ;
  }
