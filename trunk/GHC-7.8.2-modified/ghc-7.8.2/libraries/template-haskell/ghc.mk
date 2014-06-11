libraries/template-haskell_PACKAGE = template-haskell
libraries/template-haskell_dist-install_GROUP = libraries
$(if $(filter template-haskell,$(PACKAGES_STAGE0)),$(eval $(call build-package,libraries/template-haskell,dist-boot,0)))
$(if $(filter template-haskell,$(PACKAGES_STAGE1)),$(eval $(call build-package,libraries/template-haskell,dist-install,1)))
$(if $(filter template-haskell,$(PACKAGES_STAGE2)),$(eval $(call build-package,libraries/template-haskell,dist-install,2)))
