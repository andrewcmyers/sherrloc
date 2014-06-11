libraries/time_PACKAGE = time
libraries/time_dist-install_GROUP = libraries
$(if $(filter time,$(PACKAGES_STAGE0)),$(eval $(call build-package,libraries/time,dist-boot,0)))
$(if $(filter time,$(PACKAGES_STAGE1)),$(eval $(call build-package,libraries/time,dist-install,1)))
$(if $(filter time,$(PACKAGES_STAGE2)),$(eval $(call build-package,libraries/time,dist-install,2)))
