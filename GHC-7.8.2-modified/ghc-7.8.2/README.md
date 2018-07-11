# SHErrLoc for GHC

Static Holistic Error Locator. See the [project page](http://www.cs.cornell.edu/projects/SHErrLoc/)
for more information.


## Build and Development

We use the [Nix package manager](https://nixos.org/nix/) for reliable,
reproducible builds. Nix is the only dependency you
have to manually install; everything else will be handled by Nix.
After [getting Nix](https://nixos.org/nix/download.html), run the following commands at the
project root:
```shell
# Start a containerized environment
nix-shell --pure
    
# Configure GHC
./boot
./configure

# Build
make
```

This only builds the "Stage 1" compiler, that is, it uses the system GHC (Nix will install
the correct version locally) to build the modified source. "Stage 2" uses the Stage 1
compiler to compile the source again, however, our modified version of the compiler
cannot build itself, so we skip this step. This means GHCi and `make install`,
among other things, will not work. The output executable is `inplace/bin/ghc-stage1`.
