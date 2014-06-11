libraries/integer-simple_PACKAGE = integer-simple
libraries/integer-simple_dist-install_GROUP = libraries
$(if $(filter integer-simple,$(PACKAGES_STAGE0)),$(eval $(call build-package,libraries/integer-simple,dist-boot,0)))
$(if $(filter integer-simple,$(PACKAGES_STAGE1)),$(eval $(call build-package,libraries/integer-simple,dist-install,1)))
$(if $(filter integer-simple,$(PACKAGES_STAGE2)),$(eval $(call build-package,libraries/integer-simple,dist-install,2)))
