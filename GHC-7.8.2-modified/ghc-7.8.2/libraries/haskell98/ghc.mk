libraries/haskell98_PACKAGE = haskell98
libraries/haskell98_dist-install_GROUP = libraries
$(if $(filter haskell98,$(PACKAGES_STAGE0)),$(eval $(call build-package,libraries/haskell98,dist-boot,0)))
$(if $(filter haskell98,$(PACKAGES_STAGE1)),$(eval $(call build-package,libraries/haskell98,dist-install,1)))
$(if $(filter haskell98,$(PACKAGES_STAGE2)),$(eval $(call build-package,libraries/haskell98,dist-install,2)))
