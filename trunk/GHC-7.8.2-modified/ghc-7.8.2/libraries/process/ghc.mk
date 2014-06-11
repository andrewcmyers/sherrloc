libraries/process_PACKAGE = process
libraries/process_dist-install_GROUP = libraries
$(if $(filter process,$(PACKAGES_STAGE0)),$(eval $(call build-package,libraries/process,dist-boot,0)))
$(if $(filter process,$(PACKAGES_STAGE1)),$(eval $(call build-package,libraries/process,dist-install,1)))
$(if $(filter process,$(PACKAGES_STAGE2)),$(eval $(call build-package,libraries/process,dist-install,2)))
