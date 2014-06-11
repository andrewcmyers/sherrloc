libraries/ghc-prim_PACKAGE = ghc-prim
libraries/ghc-prim_dist-install_GROUP = libraries
$(if $(filter ghc-prim,$(PACKAGES_STAGE0)),$(eval $(call build-package,libraries/ghc-prim,dist-boot,0)))
$(if $(filter ghc-prim,$(PACKAGES_STAGE1)),$(eval $(call build-package,libraries/ghc-prim,dist-install,1)))
$(if $(filter ghc-prim,$(PACKAGES_STAGE2)),$(eval $(call build-package,libraries/ghc-prim,dist-install,2)))
