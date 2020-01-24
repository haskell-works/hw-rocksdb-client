let
  pinnedPkgs = import ./pkgs-from-json.nix { json = ./nixpkgs-19.09-darwin.json; };
in
  { project1 = pinnedPkgs.haskellPackages.callPackage ./default.nix { };
  }

