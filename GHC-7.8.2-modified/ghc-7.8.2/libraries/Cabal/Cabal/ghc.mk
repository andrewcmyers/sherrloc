libraries/Cabal/Cabal_PACKAGE = Cabal
libraries/Cabal/Cabal_dist-install_GROUP = libraries
$(if $(filter Cabal/Cabal,$(PACKAGES_STAGE0)),$(eval $(call build-package,libraries/Cabal/Cabal,dist-boot,0)))
$(if $(filter Cabal/Cabal,$(PACKAGES_STAGE1)),$(eval $(call build-package,libraries/Cabal/Cabal,dist-install,1)))
$(if $(filter Cabal/Cabal,$(PACKAGES_STAGE2)),$(eval $(call build-package,libraries/Cabal/Cabal,dist-install,2)))
