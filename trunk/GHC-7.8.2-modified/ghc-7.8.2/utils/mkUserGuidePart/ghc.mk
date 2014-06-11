# -----------------------------------------------------------------------------
#
# (c) 2009 The University of Glasgow
#
# This file is part of the GHC build system.
#
# To understand how the build system works and how to modify it, see
#      http://ghc.haskell.org/trac/ghc/wiki/Building/Architecture
#      http://ghc.haskell.org/trac/ghc/wiki/Building/Modifying
#
# -----------------------------------------------------------------------------

utils/mkUserGuidePart_USES_CABAL           = YES
utils/mkUserGuidePart_PACKAGE              = mkUserGuidePart
utils/mkUserGuidePart_dist_PROGNAME        = mkUserGuidePart
utils/mkUserGuidePart_dist_INSTALL_INPLACE = YES

$(eval $(call build-prog,utils/mkUserGuidePart,dist,1))
