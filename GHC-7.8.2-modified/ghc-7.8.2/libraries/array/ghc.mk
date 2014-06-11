libraries/array_PACKAGE = array
libraries/array_dist-install_GROUP = libraries
$(if $(filter array,$(PACKAGES_STAGE0)),$(eval $(call build-package,libraries/array,dist-boot,0)))
$(if $(filter array,$(PACKAGES_STAGE1)),$(eval $(call build-package,libraries/array,dist-install,1)))
$(if $(filter array,$(PACKAGES_STAGE2)),$(eval $(call build-package,libraries/array,dist-install,2)))
