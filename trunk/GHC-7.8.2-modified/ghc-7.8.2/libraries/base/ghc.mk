libraries/base_PACKAGE = base
libraries/base_dist-install_GROUP = libraries
$(if $(filter base,$(PACKAGES_STAGE0)),$(eval $(call build-package,libraries/base,dist-boot,0)))
$(if $(filter base,$(PACKAGES_STAGE1)),$(eval $(call build-package,libraries/base,dist-install,1)))
$(if $(filter base,$(PACKAGES_STAGE2)),$(eval $(call build-package,libraries/base,dist-install,2)))
