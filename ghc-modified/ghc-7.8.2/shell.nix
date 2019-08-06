# Loaded when calling `nix-shell`
let
  pkgs = import <nixpkgs> { system = builtins.currentSystem; };
in
  (import ./default.nix {inherit pkgs;})
