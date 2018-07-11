{ pkgs }:

let
  deps = {
    ghc = pkgs.stdenv.mkDerivation rec {
      name = "ghc-${version}";
      version = "7.8.2";

      src = pkgs.fetchurl {
        url = "https://downloads.haskell.org/~ghc/7.8.2/ghc-7.8.2-x86_64-apple-darwin-mavericks.tar.xz";
        sha256 = "0v0k8fszy2k4fg109047lbbkhmj7djbw810494mppd2q8qk8jcl8";
      };

      buildInputs = [
        pkgs.perl
        pkgs.autoconf
        pkgs.automake
      ];

      dontBuild = true;
      dontStrip = true;
    };

  };
in
pkgs.stdenv.mkDerivation rec {
  name = "ghc-${version}";
  version = "7.8.2-SHErrLoc";

  src = ".";

  buildInputs = [
    # Build dependencies
    pkgs.perl
    pkgs.autoconf
    pkgs.automake

    # Some Haskell libraries depend on these
    pkgs.ncurses
    pkgs.libiconv
    pkgs.gnum4

    # Standard Haskell binaries
    deps.ghc
    #pkgs.haskell.compiler.ghc784Binary
    pkgs.haskellPackages.alex
    pkgs.haskellPackages.happy
  ];

  meta = {
    description = "GHC extended with SHErrLoc";
    license = pkgs.stdenv.lib.licenses.mit;
    maintainers = [ "Andrew C. Myers <andru@cs.cornell.edu>" ];
  };
}
