libraries/binary_PACKAGE = binary
libraries/binary_dist-install_GROUP = libraries
$(if $(filter binary,$(PACKAGES_STAGE0)),$(eval $(call build-package,libraries/binary,dist-boot,0)))
$(if $(filter binary,$(PACKAGES_STAGE1)),$(eval $(call build-package,libraries/binary,dist-install,1)))
$(if $(filter binary,$(PACKAGES_STAGE2)),$(eval $(call build-package,libraries/binary,dist-install,2)))
