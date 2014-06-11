libraries/directory_PACKAGE = directory
libraries/directory_dist-install_GROUP = libraries
$(if $(filter directory,$(PACKAGES_STAGE0)),$(eval $(call build-package,libraries/directory,dist-boot,0)))
$(if $(filter directory,$(PACKAGES_STAGE1)),$(eval $(call build-package,libraries/directory,dist-install,1)))
$(if $(filter directory,$(PACKAGES_STAGE2)),$(eval $(call build-package,libraries/directory,dist-install,2)))
