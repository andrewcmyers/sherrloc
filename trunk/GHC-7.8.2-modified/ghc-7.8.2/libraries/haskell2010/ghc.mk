libraries/haskell2010_PACKAGE = haskell2010
libraries/haskell2010_dist-install_GROUP = libraries
$(if $(filter haskell2010,$(PACKAGES_STAGE0)),$(eval $(call build-package,libraries/haskell2010,dist-boot,0)))
$(if $(filter haskell2010,$(PACKAGES_STAGE1)),$(eval $(call build-package,libraries/haskell2010,dist-install,1)))
$(if $(filter haskell2010,$(PACKAGES_STAGE2)),$(eval $(call build-package,libraries/haskell2010,dist-install,2)))
